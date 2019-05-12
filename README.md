# Erlang Pollution Project

## 1. Basic pollution module

**pollution.erl** module allows to store and operate on data related to air pollution measurement.

Exported API:
```
-export([
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumStationGrowthTime/4,
  findStationByName/2,
  findStationByLocation/2]).
  ```
  
Basic API allows user to
* Add stations to the monitor
* Add measurement values to the monitor
* Remove measurement values from the monitor
* Access measurement values stored by monitor
* Calculate station's mean of given measured parameter (i. e. PM10)
* Calculate daily mean of given measured parameter (on every monitored station)
* Calculate when was the hour in which there has been noticed the highest measurement value growth on given station

## 2. Pollution server and supervisor

**pollution_server.erl** module allows to run basic pollution module as a server.
It spawns a new process which performs operations from basic **pollution.erl** module

```
-export([start/0, stop/0, crash/0, server_loop/1, ... ]).
 ```

**pollution_server_sup.erl** module acts like a supervisor for pollution server.
It spawns a new server's process whenever the previous instance of the server terminates.

```
-export([start_supervisor/0, supervisor_init/0, supervisor_loop/0, stop_supervisor/0]).
```

## 3. Behaviours: gen_server and supervisor

**pollution_gen_server.erl** module is an implementation of an _gen_server_ behaviour (OTP pattern).

**pollution_gen_server.erl** module is an implementation of an _supervisor_ behaviour (OTP pattern).