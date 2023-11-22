<br>

<p align="center">
    <a href="https://shortishly.github.io/metrics/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fmetrics%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/metrics/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP" src="https://img.shields.io/badge/Erlang%2FOTP%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/metrics?style=flat-square">
    </a>
</p>

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
