Author:Emma (Evan) Robirts
ID: 811288046
Author: Harshith Garla
ID: 811825533
Author: Shantrisse Broughton
ID: 811124860
Class: CSCI 4210 Simulation and Modeling
Project 3: Process Interaction Simulation

CREDITS:
Scalation 2.0, Authored by John A. Miller
Scala 3, from Scala Center

Wendys vs Mcdonalds: Surprisingly, the results of the process interaction simulation shows that the Multi-queue line tends to have
much more unstable and longer Queues than a single Queue system. With the Multi-queue, the Mean Wait time and Std Deviation both hover around
20, for both Queues. Meanwhile, the average wait time and the standard deviation sit around 10, which not only means the average wait is shorter,
but that wait times rarely exceed 20, which is the average wait time for the multiqueue.

Machine Shop: The Second Policy was dropped, no comparisons to be made between the systems

Vehicle Traffic Simulation: Compared to even the samples used to generate the Poisson Processes, the numbers from the Simulation were extremely low.
It's difficult to tell if this is because of the time scale of the simulation, or some deep inaccuracy within the Random Variates Used to find the results
However even with a number of cars in the simulation exceeding 40,000 the Maximum came nowhere close to the true maximums measured by the Caltrans Sensors.
From adjusting values, it would seem that speed seems to be a major determining factor in the statistics recorded by the Junctions. Therefore it's
reasonable to conclude the main way to improve our simulation would be to find a more accurate measurement for the speed random variates