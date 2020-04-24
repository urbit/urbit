import * as React from 'react'
import { BrowserRouter as Router, Route, Switch, Link, useParams, useRouteMatch } from "react-router-dom";
import { Col, Text } from "@tlon/indigo-react";

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';

export default class Chat extends React.Component {
  constructor(props) {
    super(props);

    // this.state = store.state;
    this.totalUnreads = 0;
    // store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    window.title = 'OS1 - Chat'
  }

  render() {
    console.log(this)
    const path = this.props.match.path
    return (
    <div>
      <Col>
        <Link to={`${this.props.match.url}/new`}>new</Link>
        <Link to={`${this.props.match.url}/new/dm/~zod`}>new/dm/~zod</Link>
      </Col>

    <Switch>
      <Route exact path={`${this.props.match.path}/`}>
        <h3>Select, create, or join a chat to begin.</h3>
      </Route>
      <Route path={`${this.props.match.path}/new`}>
          <h3>Create new chat</h3>
        <div />
      </Route>
      <Route path={`${this.props.match.path}/new/dm/:ship`}>
        <h3>New DM: {JSON.stringify(this.props, null, '  ')}</h3>
        {
          // new DM
        }
        <div />
      </Route>
      <Route path={`${this.props.match.path}/join/(~)?/:ship?/:station?`}>
        {
          // Join a DM
        }
        <div />
      </Route>
      <Route path={`${this.props.match.path}/(popout)?/room/(~)?/:ship/:station+`}>
        {
          // The chat stream itself
        }
        <div />
      </Route>
      <Route path={`${this.props.match.path}/(popout)?/members/(~)?/:ship/:station+`}>
        {
          // Members view of the chat
        }
        <div />
      </Route>
      <Route path={`${this.props.match.path}/(popout)?/settings/(~)?/:ship/:station+`}>
        {
          // Chat setting
        }
        <div />
      </Route>
    </Switch>
    </div>
  )
  }
}
