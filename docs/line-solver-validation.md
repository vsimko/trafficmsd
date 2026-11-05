# LINE Solver QPN validation (issue #79)

**Status:** reproducible proof-of-concept implemented; CI result pending  
**Scope:** one open, single-class QPN; no claim of full QPME/SimQPN parity  
**Pinned package:** `line-solver==3.0.8.0` (PyPI release, 2026-10-05)  
**Runtime:** Python 3.11+; CI uses Python 3.12 on Ubuntu x86-64  
**License:** BSD-3-Clause for LINE itself

## Decision

LINE has real QPN constructs, not merely ordinary queueing-network support. Its Python API exposes `Place` and `Transition`; assigning a service distribution to a `Place` turns it into a QPN queueing place. LINE's own example `spn_queueing_place.py` uses this construct and explicitly runs it with `SolverLDES`.

The TrafficMSD integration therefore uses the LDES discrete-event engine and a real queueing place. It does not replace the QPN with a conventional `Queue` node or infer QPN semantics from an ordinary queueing network.

## Reproducible headless experiment

Run from the repository root:

```sh
python -m pip install "line-solver==3.0.8.0"
python scripts/line_qpn_smoke.py
```

The experiment is defined by:

- [model-spec.json](../experiments/line-qpn-v1/model-spec.json)
- [arrival-schedule.json](../experiments/line-qpn-v1/arrival-schedule.json)
- [arrival-interarrivals.txt](../experiments/line-qpn-v1/arrival-interarrivals.txt)
- [smoke runner](../scripts/line_qpn_smoke.py)

The model is:

```text
Source -- deterministic arrivals --> Queueing Place (FCFS, 1 server)
      --> immediate completion transition --> Sink
```

The source is an open class whose inter-arrival trace is replayed from the committed file: one arrival at t=2 s and one every 2 s thereafter. The queueing place has deterministic service time 1 s. The immediate transition transfers completed tokens to the sink. This is a deliberately small QPN: the service discipline and service process belong to the queueing place itself. The runner uses LINE's trace-replay distribution for arrivals rather than converting the schedule into an exponential process.

The input schedule and authored model specification are committed. The runner serializes the constructed LINE model to the native LINE JSON interchange format and computes SHA-256 hashes for the serialized model, model specification, and arrival schedule. It runs 10,000 service-completion events with seed 23000 and emits machine-readable `result.json` plus the serialized model under `artifacts/line-qpn-v1/`.

### Hand calculation

For deterministic inter-arrivals of 2 s and deterministic service of 1 s:

- arrival rate: `lambda = 0.5 jobs/s`;
- service time: `S = 1 s`;
- utilization: `rho = lambda * S = 0.5`;
- no waiting occurs because each service finishes before the next arrival;
- mean number of jobs in the queueing place: `lambda * S = 0.5`;
- response/residence time in the queueing place: `1 s`;
- throughput: `0.5 jobs/s`.

The smoke runner checks `QLen`, `Util`, `RespT`, `ResidT`, `ArvR`, and `Tput` against these values with an absolute tolerance of 0.03. The schedule and service process are deterministic; the seed is still recorded to make the simulator invocation explicit and future-proof. Both a direct `Det` source-arrival distribution and the explicit `Replayer` trace were rejected by the pinned native C++ engine with `map_sample: D0 and D1 disagree`. The serialized model confirms that the source is represented as `Replayer` with a trace-file parameter; the native engine nevertheless enters its MAP sampler with inconsistent matrices. CI therefore forces the official JVM LDES engine so that the same QPN model and schedule can be tested independently of the native C++ execution path.

## Headless interface and runtime

The supported integration surface for this proof-of-concept is LINE's Python `SolverLDES` API, not GUI automation and not an MCP server.

The upstream implementation serializes the Python model to `model.json`, launches an LDES command-line subprocess, and parses its JSON result. On Linux it normally prefers the bundled native `line_solver/bin/ldes` executable from the PyPI wheel. The JVM implementation is selected when the native binary is unavailable or explicitly disabled with `LINE_LDES_FORCE_JAR=1`; it requires Java and may download `ldes.jar` from the upstream release. In the pinned 3.0.8.0 wheel, a native runtime error does not guarantee that a JAR fallback is available: if no JAR is already present, the wrapper can have only the native runner. TrafficMSD therefore tests the JVM engine explicitly in CI rather than assuming automatic runtime fallback. The solver creates a temporary model/result exchange directory; TrafficMSD additionally persists the model and normalized experiment result as CI artifacts.

The generic `line-cli.py` is a separate command-line surface for LINE's native JSON, JMT JSIMG, and LQNS LQNX model formats. For this QPN proof-of-concept, use `SolverLDES`: it constructs and serializes the QPN object model and invokes the LDES engine directly.

## Construct coverage

| Construct / capability | Evidence in this proof | Status |
|---|---|---|
| Petri-net places | Source, queueing place and sink are represented in a LINE network | Exercised |
| Transitions | Immediate completion transition with enabling and firing outcome | Exercised |
| QPN queueing place | `Place.set_service(class, distribution)` enables embedded queue semantics | Exercised |
| Token classes / colours | One `OpenClass` token colour | Single-colour only |
| Queue discipline | FCFS, one server | Exercised |
| Other QPN queueing-place disciplines | Native LDES queueing-place implementation explicitly accepts FCFS, LCFS, SIRO and INF; other disciplines are refused by that embedded-queue path | Known native-engine limitation |
| Routing | Deterministic linear source → place → transition → sink path | Exercised |
| Service-time distribution | Deterministic service time | Exercised |
| Arrival process | Deterministic periodic open-class source, replayed from a trace file | Exercised |
| Batch arrivals | Not used in this model | Not verified |
| Multiple token colours / class-dependent service | Not used | Not verified |
| Other scheduling disciplines / multi-server places | Not used | Not verified |
| Capacity, blocking, drop rules and inhibitor arcs | Not used | Not verified |
| Probabilistic/state-dependent routing and conflict resolution | Not used | Not verified |
| Required metrics | QLen, Util, RespT, ResidT, ArvR, Tput | Asserted by smoke test |

“Not verified” is deliberately distinct from “unsupported”: this small experiment does not establish those capabilities or their semantics. The native LDES QPN queueing-place implementation explicitly supports FCFS, LCFS, SIRO and infinite-server (INF) disciplines; it refuses other queueing-place disciplines rather than silently approximating them. Direct `Det` source-arrival encoding also failed in the pinned 3.0.8.0 native run (see the reproducibility note); the explicit `Replayer` trace path is used instead. Other LINE SPN/QPN constructs may be supported, but TrafficMSD will only advertise them after a focused fixture and semantic test exercise them. Do not silently map an unverified construct to a different LINE construct.

## Solver selection and limitations

- `SolverLDES` is the chosen execution engine for the QPN case. LINE documents LDES as its discrete-event simulator and explicitly documents queueing places as QPN semantics.
- The documented `spn_queueing_place.py` example uses LDES for a QPN with two queueing places and cross-checks an equivalent finite-population queueing network with MVA. That is useful upstream evidence, but it is not a substitute for TrafficMSD's own open-source fixture.
- Other LINE solvers must not be assumed to implement the same QPN queueing-place semantics. In particular, do not substitute an analytical solver for LDES unless its documented applicability and semantic equivalence are established.
- This experiment is a workload-level QPN performance model. It does not simulate packet-level TCP/IP, routing protocols, links, or congestion control.

## Version, licensing and provenance

- PyPI package: `line-solver==3.0.8.0`; the PyPI release page lists Python >=3.11 and release date 2026-10-05.
- PyPI wheel: `line_solver-3.0.8.0-py3-none-any.whl`, SHA-256 `676e1b26a6507fbadd155359dde5de41f26d82066d7fd5f005b4456361066809`.
- Upstream source: [imperial-qore/line-solver](https://github.com/imperial-qore/line-solver).
- Upstream license: [BSD 3-Clause](https://github.com/imperial-qore/line-solver/blob/main/LICENSE).
- Upstream third-party inventory: [THIRD-PARTY-NOTICES.md](https://github.com/imperial-qore/line-solver/blob/main/THIRD-PARTY-NOTICES.md). LINE distinguishes its BSD-3 code from separately invoked tools and the native engine's linked dependencies. This proof-of-concept uses LDES and does not invoke the GPL-licensed JMT wrapper.

The upstream repository's working tree may advertise a newer development version than the latest published PyPI wheel. TrafficMSD pins the published 3.0.8.0 release rather than an untagged development state.

## CI

[LINE QPN smoke test workflow](../.github/workflows/line-qpn-smoke.yml) installs the pinned package and Java 21, sets `LINE_LDES_FORCE_JAR=1`, runs the headless experiment, checks the deterministic metrics, and uploads the model and result JSON as an artifact. The native C++ engine is not silently substituted for the JVM engine. The result is considered verified only after a successful GitHub Actions run; a source-level review alone is not evidence that the runtime experiment passed.

## Upstream references

- [LINE project overview and supported models](https://line-solver.sourceforge.net/)
- [LINE Python API: Place / Transition / queueing-place semantics](https://line-solver.sourceforge.net/sphinx/api/core.html)
- [LINE's QPN queueing-place example](https://github.com/imperial-qore/line-solver/blob/main/python/examples/basic/stochPetriNet/spn_queueing_place.py)
- [LINE LDES solver documentation](https://line-solver.sourceforge.net/solver-ldes.html)
- [PyPI release and runtime requirements](https://pypi.org/project/line-solver/3.0.8.0/)
