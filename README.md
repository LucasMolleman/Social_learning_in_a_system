# Social_learning_in_a_system
Simulations on efficiency of social learning in a cultural system

I wrote a super simple R code that sets up a cultural 'system' in which an individual can only acquire a ('child') trait when it already has acquired its 'parent' trait. All agents start with a 'root' trait, and over the simulation can learn all traits in the 'tree'. 

The code spits out some graphs that vary in the 'branching' parameter of the system, that is, how many children each trait has. In an extreme case, this is 1 (completely vertical tree). It can also be really high, which would mimic the 'atomic' view of culture.

NB: learning is now overly simple. I will keep adding updates as I go along.

Future steps:
- add a simple way to implement payoffs for traits
- find a way to systematically analyse different learning strategies
