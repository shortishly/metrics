# Metrics

![main](https://github.com/shortishly/metrics/actions/workflows/main.yml/badge.svg)

An Erlang/OTP [Prometheus](https://prometheus.io) client application
that providing the following [metric
types](https://prometheus.io/docs/concepts/metric_types/):

- Counter. A counter is a cumulative metric that represents a single
  monotonically increasing counter whose value can only increase or be
  reset to zero on restart.
  
- Gauge. A gauge is a metric that represents a single numerical value
  that can arbitrarily go up and down.
  
- Histogram. A histogram samples observations (usually things like
  request durations or response sizes) and counts them in configurable
  buckets. It also provides a sum of all observed values.

Metrics are exposed using the [text based exposition
format](https://prometheus.io/docs/instrumenting/exposition_formats/)
via HTTP using
[Cowboy](https://ninenines.eu/docs/en/cowboy/2.9/guide/).

Internally metrics uses
[atomics](https://www.erlang.org/doc/man/atomics.html) and
[counters](https://www.erlang.org/doc/man/counters.html) available
since OTP21.2.

## Overview

The data model follows that of
[Prometheus](https://prometheus.io/docs/concepts/data_model/). Every
time series is uniquely identified by its metric name and optional
key-value pairs called labels.

The API has a single arity map parameter. While counters, gauges or
histograms are not initialised before use, once a metric name has been
used it cannot be used by another type.

```erlang
1> metrics:counter(#{name => errors, value => 6}).
ok

2> metrics:value(#{name => errors}).
6

3> metrics:counter(#{name => requests_served,
                   label => #{channel => mobile},
                   delta => 11}).
ok

4> metrics:value(#{name => requests_served,
                   label => #{channel => mobile}}).
11


5> metrics:counter(#{name => requests_served,
                   label => #{channel => desktop},
                   delta => 56}).
ok

6> metrics:value(#{name => requests_served,
                   label => #{channel => desktop}}).
56
```


## Counters

To set the value of `counter` use:

```erlang

```
