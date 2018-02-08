

Automated Extraction of Network Traffic Models Suitable for Performance Simulation
========================================================
author: Viliam Simko
date: 16.10.2015
autosize: true

[FZI]: http://www.fzi.de "Forschungszentrum Informatik"
[UWU]: http://https://www.uni-wuerzburg.de "Universität Würzburg"
[ZfT]: http://www.telematik-zentrum.de "ZfT Zentrum für Telematik"

Authors             | Institutions
--------------------|----------------------------------------
Viliam Simko        | [FZI Forschungszentrum Informatik][FZI]
Piotr Rygielski     | [Universität Würzburg][UWU]
Samuel Kounev       | [Universität Würzburg][UWU]
Klaus Schilling     | [Universität Würzburg][UWU]
Felix Sittner       | [ZfT Zentrum für Telematik][ZfT]
Doris Aschenbrenner | [ZfT Zentrum für Telematik][ZfT]


Context and Motivation
========================================================
**Nowadays data centers**
- Increasingly dynamic due to virtualization
- High intra-data-center traffic (migration of VMs)

**We focused on**
- Modeling performance of network in a data center
- Network throughput
- Assuming signals with some periodic component

**Currently**
- Architecture derived automatically
- **Traffic models created manually**


Requirements
========================================================
- Derive automatically
- Decompose into generators / intervals
- Minimize the model size
- Preserve signal shape
- Tunable trade-off: precision / model size


High-level overview
========================================================
TODO Image $\rightarrow$ Simulation
![](images/overview-schema.png)


DML + DNI (Descartes Modeling Language)
========================================================
High-level language → simulation models

**Simulators**: OMNeT++, 2 x QPN
- Each simulator uses different abstractions
- Each requires specific knowledge

**Tradeoff**: accuracy vs. simulation overhead


Related Work
========================================================
**Probabilistic models**
- TODO link
- TODO link
- TODO link

**LIMBO**
- A load Intensity modeling platform


Typical traffic dumps
========================================================
![plot of chunk dumps](images-gen/dumps-1.png)


Typical traffic dumps - densities
========================================================
![plot of chunk densities](images-gen/densities-1.png)


Model of a simple traffic generator
========================================================
$G = (scale, amplitude, begin, end)$

Parameter     | Description
--------------|-------------------------------------------------
**scale**     | Generator emits packets every $2^{scale}$ seconds.
**amplitude** | Size of the packet to be emitted.
**begin**     | Beginning of the activity
**end**       | End of the activity


Decomposition into traffic generators
========================================================
TODO Image


Example: 10 minutes of traffic - before optimization
========================================================




```
Error in decompose(.) : time series has no or less than 2 periods
```
