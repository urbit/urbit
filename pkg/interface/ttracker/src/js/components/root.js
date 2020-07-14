import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import _ from 'lodash';
import { HeaderBar } from "./lib/header-bar.js"
import {Home} from "./Home"
import {Stations} from "./Stations"
import { Alerts } from './Alerts.js';
import { Planner } from './Planner.js';
import { Schedule } from './Schedule.js'
import {Station} from './Station.js'
import {Splash}  from './Splash'
import { api } from '../api';
import {PlannerResult} from './PlannerResult'
import { min } from 'date-fns';


export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
  }

  componentDidMount() {
    store.setStateHandler((newState) => {
        this.setState(newState);
    });
    
  }

  render() {
    if(!this.state.alertsLoaded || !this.state.stationsLoaded || !this.state.routesLoaded ) {
      return null;
    }
    
      
  return (
      <BrowserRouter>
        <div className="absolute h-100 w-100 bg-gray0-d ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
          <HeaderBar>
          </HeaderBar>
          <div className="cf w-100 flex flex-column pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d overflow-auto">
            <Link to='/~ttracker'>Home</Link>
             <Route exact path="/~ttracker" render={ () => {
              return (
            <Home/>
                )}}
            />
            <Route exact path="/~ttracker/stations" render={ () => {
              
              return (
                <Stations stops={this.state.stations} routes={this.state.routes}/>
               )}}
            />
            <Route exact path="/~ttracker/alerts" render={ () => {
              if(this.state.alerts )
              return(
                <Alerts alerts={this.state.alerts} routes={this.state.routes}/>
              )}}
              />
              <Route exact path="/~ttracker/schedule" render={ () => {
              return(
                <Schedule routes={this.state.routes}>
                  </Schedule>
              )}}
              />
              <Route exact path="/~ttracker/planner" render={ () => {
              return(
                <Planner stops={this.state.stations}/>
              )}}></Route>
              <Route path="/~ttracker/stations/:stationName" render={ () => {
                return(
                  <Station facilities={this.state.facilities} stations={this.state.stations}/>
                )}}>
              </Route>
          </div>
        </div>
      </BrowserRouter>
    )
  }
}

