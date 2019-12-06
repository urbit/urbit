import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/lib/new';
import { Contacts } from '/components/contacts';
import { ContactCard } from '/components/lib/card'


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
    this.setSpinner = this.setSpinner.bind(this);
  }

  setSpinner(spinner) {
    this.setState({
      spinner
    });
  }

  render() {
    const { props, state } = this;

    return (
      <BrowserRouter>
        <div className="h-100 w-100">
        <Route exact path="/~contacts"
          render={ (props) => {
            return (
              <Skeleton activeDrawer="groups" contacts={state.contacts}>
                <div className="h-100 w-100 overflow-x-hidden bg-gray0 dn db-ns"></div>
              </Skeleton>
            );
          }} />
          <Route exact path="/~contacts/new" 
            render={ (props) => {
              return (
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel">
                  <NewScreen />
                </Skeleton>
              );
          }} />
          <Route exact path="/~contacts/:ship/:group"
            render={ (props) => {
              let groupPath = `/${props.match.params.ship}/${props.match.params.group}`;

              let contactList = state.contacts[groupPath];

              return(
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="contacts"
                  selected={groupPath}>
                    <Contacts 
                    contacts={contactList}
                    path={groupPath} />
                    <div className="h-100 w-100 overflow-x-hidden bg-gray0 dn db-ns"></div>
                  </Skeleton>
              )
            }}
            />
            <Route exact path="/~contacts/:ship/:group/:contact"
            render={ (props) => {
              let groupPath = `/${props.match.params.ship}/${props.match.params.group}`;
              let thisContactPath = `/${props.match.params.ship}/${props.match.params.group}/${props.match.params.contact}`

              let contactList = state.contacts[groupPath];

              return(
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected={groupPath}>
                    <Contacts 
                    contacts={contactList}
                    path={groupPath}
                    selectedContact={thisContactPath} />
                    <ContactCard/>
                  </Skeleton>
              )
            }}
            />
            <Route exact path="/~contacts/me"
            render={ (props) => {
              return(
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected="me">
                    <ContactCard/>
                  </Skeleton>
              )
            }}
            />
        </div>
      </BrowserRouter>
    )
  }
}

