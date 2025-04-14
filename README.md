# Bitcoin-Alpha-trust-network-Analysis

### Data description

The Bitcoin Alpha trust-weighted signed network is a dataset about the who-trusts-whom
network of people who trade using Bitcoin on a Bitcoin Alpha platform.
Since Bitcoin users are anonymous, a record of their reputations is needed to prevent
transactions with fraudulent and risky users. Members of Bitcoin Alpha rate other members on
a scale of -10 (total distrust) to +10 (total trust) in steps of 1.

source: <a> https://snap.stanford.edu/data/soc-sign-bitcoin-alpha.html </a>

We used the following features in our analysis:

- Source: node id of source ( Rater)
- Target: node id of the target (Ratee)
- Rating: the source's rating for the target (ranging from -10 to +10 in step of 1)

### Desired output

- Understanding the Network's Trust Dynamics.
- Identifying Influential Nodes.

### Approach

● Network Data Preprocessing and Representation
● Social Network Analytics Process
○ Centrality Measures
○ Connectivity Analysis
○ Special Graphs
○ Quantifying Social Structure

### My contribution:

##### Quantifying Social Structure

1. Path Length Analysis (How easily users can connect based on trust?)
2. Clustering coeffcient (How densely connected is the network?)

##### Interactive Network Representation

Creating an interactive network visualization using the visNetwork package, where:
● Node colors: average of incoming ratings (red for negative “distrust” & Blue for
positive “trust”).

● Node sizes: average transformed weight of incoming rating (indicating the higher
ratings).

### Interpretation and Insights

Summary of Findings:
● Most users exhibit low connectivity, rating or being rated by only a few others. This
highlights a preference for trust-focused, selective relationships rather than broad
interaction.

● The network is weakly connected, with influence concentrated among a few key
nodes acting as bridges, indicating limited central coordination.

● A large connected component enables trust flow for the majority of users, but smaller components
disconnected groups persist, representing isolated or inactive segments.

● The low average path length reflects an efficient network of users with mutual trust.
With many nodes are pendants or isolated, leading to limited dense trust communities.

“The Bitcoin Alpha network shows that people tend to interact with a small, trusted group rather than engaging broadly. Most activity happens within a main group of users, while smaller, disconnected groups represent individuals who are less active or isolated. Trust spreads quickly among connected users, but many people interact only minimally, leading to fewer tightly connected communities. This structure highlights opportunities to bring users into the fold and encourage broader participation to strengthen relationships and collaboration.”
