import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { makeRoutePath } from '../lib/util';
import urbitOb from 'urbit-ob';
import { Subscriptions } from '../components/subscriptions';
import { SearchableList } from '../components/searchable-list';
import { Summary } from '../components/summary';

export class Apps extends Component {

  constructor(props) {
    super(props);
    this.state = {};

    this.loadApps = this.loadApps.bind(this);
    this.loadAppDetails = this.loadAppDetails.bind(this);
  }

  componentDidMount() {
    if (Object.keys(this.props.apps).length === 0) {
      this.loadApps();
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadApps() {
    api.getApps();
  }

  loadAppDetails(app) {
    api.getAppDetails(app);
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;

    console.log('render', props.apps, Object.keys(props.apps));
    const apps = Object.keys(props.apps).sort().map(app => {
      const appData = props.apps[app];
      const haveDeets = (typeof appData === 'object');
      const running = haveDeets
        ? true
        : appData;
      const runStyle = running
        ? {borderLeft: '3px solid green'}
        : {borderLeft: '3px solid grey'}

      let deets = null;
      if (!haveDeets) {
        deets = running
          ? "Loading..."
          : "App not running.";
      } else if (appData.noDebug) {
        deets = "App doesn't use /lib/dbug";
      } else {
        const data = appData;
        const events = (data.events || []).map(e => {
          return {key: e, jsx: (<>
            {e}<br/>
          </>)};
        })
        deets = (<>
          <button
            style={{position: 'absolute', top: 0, right: 0}}
            onClick={()=>{this.loadAppDetails(app)}}
          >
            refresh
          </button>
          <div style={{maxHeight: '500px', overflow: 'scroll'}}>
            <pre>{data.state.join('\n')}</pre>
          </div>
          <div>
            <Subscriptions {...data.subscriptions} />
          </div>
          <div>
            <button onClick={()=>{api.bindToVerb(app)}}>listen to verb</button>
            <SearchableList placeholder="event description" items={events} />
          </div>
        </>)
      }

      const onOpen = running
        ? this.loadAppDetails
        : null;

      return {key: app, jsx: (
        <Summary id={app} summary={'%'+app} details={deets} onOpen={onOpen} style={runStyle} />
      )};
    });

    return (
      <div
        className={
          "h-100 w-100 pa3 pt4 overflow-x-hidden " +
          "bg-gray0-d white-d flex flex-column"
        }>
        <SearchableList placeholder="app name" items={apps}>
          <button onClick={this.loadApps}>refresh</button>
        </SearchableList>
      </div>
    );
  }
}
