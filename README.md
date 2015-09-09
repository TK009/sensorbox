
sensorbox - core
================

This is a part of Internet of Things (IoT) server architecture with simplicity and
modularity in mind. It aggregates sensor data from external programs and routes
data through single core logic point where it can be filtered  by `Subscriptions`
and sent to `Callback` addresses over TCP.

So all data should be sent to this core component which sends it to any number
of components. Inputs could be sensor devices or virtual sensor aggregations and
services could be databases, REST HTTP services or even controllers for
physical devices. For some examples see [Relevant
Components](#relevant-components)

This project follows the basic ideas of the Open Messaging Interface (O-MI) and
Open Data Format (O-DF) standards but implements its own simplified protocol on
TCP. This program is supposed to be only the core and all kinds of services
should be built around it, including but not limited to O-MI and O-DF.

Status of developement
----------------------

Usable for testing. Not yet ready for production. Missing parts of the system:
* Communication
  * Better Response sensor data format
* Subscriptions
  * Removing expired event subs
* Persistency controller
  * Create db checkpoints (otherwise restarting might take increasingly long)

TODO
-----

* A way to inspect current subscriptions
* Logging
* More documentation
* Tests?


Features
--------

* Supports the idea of Objects and InfoItems in the form of a path (O-DF)
  - Path is slash '/' delimited string that does not end or start with a slash
    * E.g. "Objects/MyHouse/Floor1/LivingRoom/Temperature"
  - This allows one to categorize sensors into directories and make a hierarchy.
  - A common root named "Objects" is recommended for convenience as in O-DF
    * Enables possibilty to subscribing to all sensors (e.g. for detecting any
      new sensors)
    * Easier compatibility for O-DF
* Supports a subset of Subscriptions as defined in O-MI
  - Interval based subscriptions; Triggered at intervals and sends the latest
    data from memory for requested sensors.
  - Event based subscriptions; Triggered when certain event occurs on the
    sensor. Currently supported events:
    * `OnAttach`: When receiving the first value from the sensor
    * `OnUpdate`: When there is any value received for the sensor
    * `OnChange`: When there is a new value received and it is different than
      the last value.
  - Event based subscriptions supports also parent level subscriptions, meaning
    that future sensors below subscribed parent will also trigger the events
  - Subscriptions can be removed by waiting them to expire after the given time
    or with Cancel request.
    * `RequestID` is returned when a subscription is made
* Write request is the only way to push new data to this server
* Cancel request can stop and remove a subscription with its RequestID
* For more info about the protocol and special requests, see source file
  Protocol.hs


Use cases - How can this be used to do things?
----------------------------------------------

TODO: This part will be filled later


Relevant Components
-------------------

* Connecting to arduino with firmata: 
  [arduinoserv](https://github.com/TK009/arduinoserv)

* Planned: A simple MetaData service for providing metadata about, configuring and setting up devices and
  subscriptions from a single point.

Licence
-------
MIT, see file `LICENCE`

