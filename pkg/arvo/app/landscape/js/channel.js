class Channel {
  constructor() {
    this.init();
    this.deleteOnUnload();

    //  a way to handle channel errors
    //
    //
    this.onChannelError = (err) => {
      console.error('event source error: ', err);
    };
    this.onChannelOpen = (e) => {
      console.log('open', e);
    };
  }

  init() {
    this.debounceInterval = 500;
    //  unique identifier: current time and random number
    //
    this.uid =
      new Date().getTime().toString() +
      "-" +
      Math.random().toString(16).slice(-6);

    this.requestId = 1;

    //  the currently connected EventSource
    //
    this.eventSource = null;

    //  the id of the last EventSource event we received
    //
    this.lastEventId = 0;

    //  this last event id acknowledgment sent to the server
    //
    this.lastAcknowledgedEventId = 0;

    //  a registry of requestId to successFunc/failureFunc
    //
    //    These functions are registered during a +poke and are executed
    //    in the onServerEvent()/onServerError() callbacks. Only one of
    //    the functions will be called, and the outstanding poke will be
    //    removed after calling the success or failure function.
    //

    this.outstandingPokes = new Map();

    //  a registry of requestId to subscription functions.
    //
    //    These functions are registered during a +subscribe and are
    //    executed in the onServerEvent()/onServerError() callbacks. The
    //    event function will be called whenever a new piece of data on this
    //    subscription is available, which may be 0, 1, or many times. The
    //    disconnect function may be called exactly once.
    //
    this.outstandingSubscriptions = new Map();

    this.outstandingJSON = [];

    this.debounceTimer = null;
  }

  resetDebounceTimer() {
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
      this.debounceTimer = null;
    }
    this.debounceTimer = setTimeout(() => {
      this.sendJSONToChannel();
    }, this.debounceInterval)
  }

  setOnChannelError(onError = (err) => {}) {
    this.onChannelError = onError;
  }

  setOnChannelOpen(onOpen = (e) => {}) {
    this.onChannelOpen = onOpen;
  }

  deleteOnUnload() {
    window.addEventListener("beforeunload", (event) => {
      this.delete();
    });
  }

  clearQueue() { 
    clearTimeout(this.debounceTimer);
    this.debounceTimer = null;
    this.sendJSONToChannel();
  }

  //  sends a poke to an app on an urbit ship
  //
  poke(ship, app, mark, json, successFunc, failureFunc) {
    let id = this.nextId();
    this.outstandingPokes.set(
      id,
      {
        success: successFunc,
        fail: failureFunc
      }
    );

    const j = {
      id,
      action: "poke",
      ship,
      app,
      mark,
      json
    };

    this.sendJSONToChannel(j);
  }

  //  subscribes to a path on an specific app and ship.
  //
  //    Returns a subscription id, which is the same as the same internal id
  //    passed to your Urbit.
  subscribe(
      ship,
      app,
      path,
      connectionErrFunc = () => {},
      eventFunc = () => {},
      quitFunc = () => {},
      subAckFunc = () => {},
  ) {
    let id = this.nextId();
    this.outstandingSubscriptions.set(
      id,
      {
        err: connectionErrFunc,
        event: eventFunc,
        quit: quitFunc,
        subAck: subAckFunc
      }
    );

    const json = {
      id,
      action: "subscribe",
      ship,
      app,
      path
    }

    this.resetDebounceTimer();

    this.outstandingJSON.push(json);
    return id;
  }

  //  quit the channel
  //
  delete() {
    let id = this.nextId();
    clearInterval(this.ackTimer);
    navigator.sendBeacon(this.channelURL(), JSON.stringify([{
      id,
      action: "delete"
    }]));
    if (this.eventSource) {
      this.eventSource.close();
    }
  }

  //  unsubscribe to a specific subscription
  //
  unsubscribe(subscription) {
    let id = this.nextId();
    this.sendJSONToChannel({
      id,
      action: "unsubscribe",
      subscription
    });
  }

  //  sends a JSON command command to the server.
  //
  sendJSONToChannel(j) {
    let req = new XMLHttpRequest();
    req.open("PUT", this.channelURL());
    req.setRequestHeader("Content-Type", "application/json");

    if (this.lastEventId == this.lastAcknowledgedEventId) {
      if (j) {
        this.outstandingJSON.push(j);
      }

      if (this.outstandingJSON.length > 0) {
        let x = JSON.stringify(this.outstandingJSON);
        req.send(x);
      }
    } else {
      //  we add an acknowledgment to clear the server side queue
      //
      //    The server side puts messages it sends us in a queue until we
      //    acknowledge that we received it.
      //
      let payload = [
        ...this.outstandingJSON, 
        {action: "ack", "event-id": this.lastEventId}
      ];
      if (j) {
        payload.push(j)
      }
      let x = JSON.stringify(payload);
      req.send(x);

      this.lastAcknowledgedEventId = this.lastEventId;
    }
    this.outstandingJSON = [];

    this.connectIfDisconnected();
  }

  //  connects to the EventSource if we are not currently connected
  //
  connectIfDisconnected() {
    if (this.eventSource) {
      return;
    }

    this.eventSource = new EventSource(this.channelURL(), {withCredentials:true});
    this.eventSource.onmessage = e => {
      this.lastEventId = parseInt(e.lastEventId, 10);

      let obj = JSON.parse(e.data);
      let pokeFuncs = this.outstandingPokes.get(obj.id);
      let subFuncs = this.outstandingSubscriptions.get(obj.id);

      if (obj.response == "poke" && !!pokeFuncs) {
        let funcs = pokeFuncs;
        if (obj.hasOwnProperty("ok")) {
          funcs["success"]();
        } else if (obj.hasOwnProperty("err")) {
          funcs["fail"](obj.err);
        } else {
          console.error("Invalid poke response: ", obj);
        }
        this.outstandingPokes.delete(obj.id);

      } else if (obj.response == "subscribe" ||
                (obj.response == "poke" && !!subFuncs)) {
        let funcs = subFuncs;

        if (obj.hasOwnProperty("err")) {
          funcs["err"](obj.err);
          this.outstandingSubscriptions.delete(obj.id);
        } else if (obj.hasOwnProperty("ok")) {
          funcs["subAck"](obj);
        }
      } else if (obj.response == "diff") {
        // ensure we ack before channel clogs
        if((this.lastEventId - this.lastAcknowledgedEventId) > 30) {
          this.clearQueue();
        }

        let funcs = subFuncs;
        funcs["event"](obj.json);
      } else if (obj.response == "quit") {
        let funcs = subFuncs;
        funcs["quit"](obj);
        this.outstandingSubscriptions.delete(obj.id);
      } else {
        console.log("Unrecognized response: ", e);
      }
    }

    this.eventSource.onopen = this.onChannelOpen;

    this.eventSource.onerror = e => {
      this.delete();
      this.init();
      this.onChannelError(e);
    }
  }

  channelURL() {
    return "/~/channel/" + this.uid;
  }

  nextId() {
    return this.requestId++;
  }
}

window.channel = Channel;
