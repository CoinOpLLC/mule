### dicta:

##### The graph AI paper has the connection between `AD` and `Event Mux`

>GNNs already operate at a massive scale. Depending on the amount of data, the complexity of models, and the range of applications, GNNs can easily become huge consumers of processing, storage, I/O bandwidth, and other big-data platform resources. **If you're driving the results of graph processing into real-time applications, such as anti-fraud, you’ll need an end-to-end low-latency graph database.**

##### I keep hearing Wiltbank saying what I would say (need moar AI angle)

- it exists, as discussed, but for some reason Ryan seems shy about exploiting this.
- am content with Rob hammering on this for now, but plan to monitor
- key apps:
  - network and system log data
  - IOT
  - social/mobile/transaction stew

##### Ryan might not have built Skynet, but he built a layer of it
- entity resulution built in - seems pretty sophisticated
- _homioconicity_ (like `LISP`, but for graphs instead of lists)

- semantic caching: Wen Han's laws:
  - need flexibility? add a layer of indirection (generalization)
  - need performance? remove a layer of indirection (specialization)

- thatDot Connect starts out by comprehending "all the levels" (like other graph DB)_
- **semantic caching is truly adaptive specialization (removing a level)**

anomaly detector

lean the transform - make that part open source
order by cardinality by default
CDN example - quality metric

how high up the chain is the conditional choke? ;)
