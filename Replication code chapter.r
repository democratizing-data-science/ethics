library(igraph)
a<-read.csv("https://raw.githubusercontent.com/democratizing-data-science/ethics/main/consent.csv")
g <- graph.data.frame(a)
V(g)$color <- V(g)$name %in% a[,1]
V(g)$size <- (betweenness(g, directed=F)/max( betweenness(g, directed=F)))*40
V(g)$size <- ifelse(V(g)$size==min(V(g)$size), 5, V(g)$size)
set.seed(47)
locs <- layout.fruchterman.reingold(g)
plot(g, main="Conflict Network", sub="Simulated Example with 15 Participants", layout=locs, label=1:15)
legend('bottomleft', col=c("white", "orange"), fill=c("white", "orange"), legend=c("Non-consented (N = 3)", "Consented (N = 12)"), title="Participants' Consent")

# Remove a non-consented vertices
removals1 <- c("A2", "A5", "A9")
g2 <- induced_subgraph(g, V(g)[c( "A1", "A3", "A4", "A6", "A7", "A8", "A10", "A11", "A12", "A13", "A14", "A15")])
plot(g2, layout=(locs[-which(as_ids(V(g)) %in% removals1),]), main="Conflict Network \nAfter Removing Non-consented Actors", sub="Simulated Example with 12 Remaining Participants")
legend('bottomleft', col=c("white", "orange"), fill=c("white", "orange"), legend=c("Non-consented (N = 0)", "Consented (N = 12)"), title="Participants' Consent")
