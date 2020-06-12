import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import _ from 'lodash';
import { HeaderBar } from "./lib/header-bar.js"

function padNumber(number) {
  if (number == 0) {
    return "00";
  }
  if (number <= 9) {
    return `0${number}`
  }
  return number.toString();
}

function isSundaySchedule(curTime) {
  // Deliberately switch over the effective day in the middle of the
  // night.
  const dayOfWeek = curTime.getDay();
  const hour = curTime.getHours();
  const isSunday = (dayOfWeek === 0 && hour > 4) || (dayOfWeek === 1 && hour < 4);
  return isSunday;
}

function getOptionsFromStations(stations) {
  return _.map(stations, (station) => {
    const abbr = station.abbr;
    const name = station.name;
    return <option key={abbr} value={abbr}>{name}</option>;
  });
}

class ScheduleWidget extends Component {
  constructor(props) {
    super(props);
    this.state = { station: "" };


  }

  static getDerivedStateFromProps(props, state) {
    if (state.station === "" && props.stations && props.stations[0]) {
      const abbr = props.stations[0].abbr;
      return { station: abbr }
    }
    return null;
  }
  getSchedule(evt) {
    // Needs to make a request at https://www.bart.gov/schedules/bystationresults?station=12TH&date=06/03/2020&time=6%3A30%20PM
    const station = this.state.station;
    const t = new Date();
    const date = `${t.getMonth()}/${t.getDay()}/${t.getYear()}`

    const hours = t.getHours();
    const h = hours === 0 ? 12 : hours % 12;
    const m = padNumber(t.getMinutes());
    const meridian = hours >= 12 ? "PM": "AM"
    const timeStr = `${h}:${m} ${meridian}`;
    const url = `https://www.bart.gov/schedules/bystationresults?station=${station}&date=${date}&time=${timeStr}`;
    window.open(url, '_blank');
    evt.preventDefault();
  }

  changeStation(evt) {
    const value = evt.target.value;
    this.setState({station: value});
  }

  render() {
    const stations = this.props.stations;
    return (<div>
      <form name="getSchedule" onSubmit={this.getSchedule.bind(this)}>
        <select disabled={!stations} name="stations" value={this.state.fromStation} onChange={this.changeStation.bind(this)}>
          { getOptionsFromStations(stations) }
        </select>
        <input type="submit" value="Get schedule"/>
      </form>
    </div>);
  }
}

class ElevatorWidget extends Component {

  statuses() {
    const elevatorStatuses = this.props.elevatorStatuses;
    if (elevatorStatuses.length === 0) {
      return <p>No elevators are known to be out of service.</p>;
    }
    return (<div>
      { _.map(elevatorStatuses, (st, idx) => {
        const desc = st.description['#cdata-section'];
        return <p key={idx}>{desc}</p>;
      })
      }
    </div>);
  }

  render() {
    return (<div className="cf w-100 flex flex-column pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-s h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d overflow-x-hidden">
        <h1 className="mt0 f8 fw4">BART Info - Elevator status</h1>
        <Link to="/~bartinfo">Route planner</Link>
        { this.statuses() }
    </div>);
  }
}

class TimeScheduleWidget extends Component {
  constructor(props) {
    super(props)
    this.state = { curTime: new Date() };
  }

  componentDidMount() {
    this.timerId = setInterval(() => this.tick(), 1000);
  }
  componentWillUnmount() {
    clearInterval(this.timerId);
  }

  tick() {
    this.setState({curTime: new Date()});
  }

  render() {
    const curTime = this.state.curTime;
    const timeStr = curTime.toLocaleTimeString();
    const serviceStr = isSundaySchedule(curTime) ? "Sunday service" : "Weekday / Saturday service";
    return (<div style={{textAlign: "center"}}>
      <p className="lh-copy measure pt3">Current time: <b>{timeStr}</b></p>
      <p className="lh-copy measure pt3">{serviceStr}</p>
    </div>);
  }
}

class RouteSearch extends Component {
  constructor(props) {
    super(props);
    const now = new Date();
    const hours = now.getHours();
    this.state = {
      fromStation: "",
      toStation: "",
      depart: 'now',
      min: now.getMinutes(),
      hour: hours === 0 ? 12 : hours % 12,
      isPM: hours >= 12
    };
  }

  static getDerivedStateFromProps(props, state) {
    if (state.fromStation === "" && props.stations && props.stations[0]) {
      const abbr = props.stations[0].abbr;
      return { ...state, fromStation: abbr, toStation: abbr};
    }
    return null;
  }

  stationSearch(evt) {
    evt.preventDefault();
    api.action("bartinfo", "json", {
      from: this.state.fromStation,
      to: this.state.toStation,
      min: this.state.min,
      hour: this.state.hour,
      isPM: this.state.isPM,
    });
  }

  changeStation(evt) {
    const target = evt.target.name;
    const value = evt.target.value;
    if (target === "fromStation") {
      this.setState({fromStation: value});
    } else if (target === "toStation") {
      this.setState({toStation: value});
    }
  }

  setDepartNow(evt) {
    evt.preventDefault();
    this.setState({depart: "now"});
  }

  setDepartAt(evt) {
    evt.preventDefault();
    const now = new Date();
    const hours = now.getHours();
    this.setState({
      depart: "givenTime",
      min: now.getMinutes(),
      hour: hours === 0 ? 12 : hours % 12,
      isPM: hours >= 12
    });
  }

  renderTimePicker() {
    const state = this.state;
    const departNow = this.state.depart === 'now';
    return (<div style={{display: "flex"}}>
      <div> 
        <a href="" onClick={ this.setDepartNow.bind(this) }>
          <div>
            <p>{ departNow ? <b>Now</b> : "Now" }</p>
          </div>
        </a>
        <a href="" onClick={ this.setDepartAt.bind(this)}>
          <div>
            <p>{ departNow ? "At..." : <b>At...</b>}</p>
          </div>
        </a>
      </div>
      <div>
        <div></div>
        <div>
        </div>
          <select 
            name="hour"
            value={this.state.hour}
            onChange={(evt) => this.setState({hour: parseInt(evt.target.value)}) } disabled={departNow}
          >
            { _.map(_.range(1, 13), (hour) => { return <option key={`h-${hour}`} value={hour}>{padNumber(hour)}</option>;}) }
          </select>
            <span>:</span>
          <select
            name="min"
            value={this.state.min}
            onChange={(evt) => this.setState({min: parseInt(evt.target.value)}) } disabled={departNow}
          >
            { _.map(_.range(0, 60), (min) => { return <option key={`m-${min}`} value={min}>{padNumber(min)}</option>;}) }
          </select>
          <select 
            name="isPM"
            value={this.state.isPM ? "PM" : "AM"}
            disabled={departNow}
            onChange={(evt) => this.setState({isPM: evt.target.value === "PM"})}
          >
            <option value="AM">AM</option>
            <option value="PM">PM</option>
          </select>
      </div>
    </div>);
  }

  render() {
    const receivedStations = this.props.stations;
    return (<form name="bartSearch" onSubmit={this.stationSearch.bind(this)}>
      From:
      <select disabled={!receivedStations} name="fromStation" value={this.state.fromStation} onChange={this.changeStation.bind(this)}>
        { getOptionsFromStations(receivedStations) }
      </select>
      <br/>
      To:
      <select disabled={!receivedStations} name="toStation" value={this.state.toStation} onChange={this.changeStation.bind(this)}>
        { getOptionsFromStations(receivedStations) }
      </select>
      <div>
        Depart at:
        { this.renderTimePicker() }
      </div>
      <div style={{padding: '5px'}}>
        <input type="submit" value="Search"/>
      </div>
    </form>);
  }
}

class IndividualRouteResult extends Component {
  render() {
    const trip = this.props.trip;
    return (<div>
      Depart: {trip.depart} Arrive: {trip.arrive} ({trip.time})
      <br/>
      Cost: {trip.fare}
      <br/>
      Legs:
      { _.map(trip.legs, (leg) => `${leg.line} line`) }
    </div>);
  }
}

class RouteResults extends Component {
  render() {
    const routes = this.props.routes;
    console.log(this.props.routes);
    if (!routes) {
      return (<div></div>);
    }

    const request = routes.request;
    const trip = request.trip;
    const trips = _.map(trip, (t) => {
      return {
        fare: t['@fare'],
        depart: t['@origTimeMin'],
        arrive: t['@destTimeMin'],
        time: t['@tripTime'],
        legs: _.map(t.leg, (leg) => {
          return {line: leg['@trainHeadStation'] };
        })
      };
    });

    return (<div>
      Trains:
      <br/>
      { _.map(trips, (trip, idx) => <IndividualRouteResult key={idx} trip={trip} />) }
    </div>);
  }
}

class RoutePlanner extends Component {
  render() {
    const curTime = this.props.curTime;
    const mapFilename = isSundaySchedule(curTime) ? "BART-Map-Sunday.png" : "BART-Map-Weekday-Saturday.png";
    const mapPath=`/~bartinfo/img/${mapFilename}`;

    return (
      <div className="cf w-100 flex flex-column pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-s h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d overflow-x-hidden">
        <h1 className="mt0 f8 fw4">BART Info</h1>
        <Link to="/~bartinfo/elevators">Elevator information</Link>
        <div style={{display: "grid", gridTemplateColumns: "66% 34%", gridGap: "10px" }}>
          <div className="topinfo" style={{gridColumn: "1 / 2"}}>
            <TimeScheduleWidget/>
          </div>
          <div className="map" style={{gridColumn: "1", gridRow: "2"}}>
            <img src={mapPath} />
          </div>
          <div className="searchsidebar" style={{gridColumn: "2", gridRow: "2"}}>
            Search scheduled trains:
            <RouteSearch stations={this.props.stations} curTime={curTime} />
            <br/>
            <RouteResults routes={this.props.routes} />
            or see the official bart scheduler for a given station, date and time:
            <ScheduleWidget stations={this.props.stations}/>
          </div>
        </div>
      </div>
    );
  }
}


export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler((newState) => {
        this.setState(newState);
    });
  }

  render() {
    return (
      <BrowserRouter>
        <div className="absolute h-100 w-100 bg-gray0-d ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
          <HeaderBar/>
          <Route exact path="/~bartinfo/elevators" render={ () =>
            <ElevatorWidget
              elevatorStatuses={this.state.elevators || []}
            /> }/>
          <Route exact path="/~bartinfo" render={ () =>
            <RoutePlanner
              curTime={new Date() }
              stations={this.state.stations || []}
              routes={this.state.routes}
            /> } />
        </div>
      </BrowserRouter>
    )
  }
}

