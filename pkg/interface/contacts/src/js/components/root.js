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
import { ContactCard } from '/components/lib/card';
import { AddScreen } from '/components/lib/add-contact';


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
                  <NewScreen 
                    setSpinner={this.setSpinner}
                    api={api}
                  />
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
                    activeDrawer="contacts"
                    path={groupPath} />
                    <div className="h-100 w-100 overflow-x-hidden bg-gray0 dn db-ns"></div>
                  </Skeleton>
              )
            }}
            />
          <Route exact path="/~contacts/add/:ship/:group"
            render={(props) => {
              let groupPath = `/${props.match.params.ship}/${props.match.params.group}`;

              let contactList = state.contacts[groupPath];

              return (
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected={groupPath}>
                  <Contacts
                    contacts={contactList}
                    activeDrawer="rightPanel"
                    path={groupPath} />
                  <AddScreen 
                    path={groupPath}
                    contacts={contactList}
                  />
                </Skeleton>
              )
            }}
          />
            <Route exact path="/~contacts/:ship/:group/:contact"
            render={ (props) => {
              let groupPath = `/${props.match.params.ship}/${props.match.params.group}`;
              let thisContactPath = `/${props.match.params.ship}/${props.match.params.group}/${props.match.params.contact}`
              let contactList = state.contacts;
              contactList = (contactList !== undefined)
              ? state.contacts[groupPath]
              : {};
              let contact = (contactList !== undefined)
              ? contactList[props.match.params.contact]
              : {};

              return(
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected={groupPath}>
                    <Contacts 
                    activeDrawer="rightPanel"
                    contacts={contactList}
                    path={groupPath}
                    selectedContact={thisContactPath} />
                    <ContactCard
                    contact={contact}
                    path={groupPath}
                    ship={props.match.params.contact}
                    />
                  </Skeleton>
              )
            }}
            />
          <Route exact path="/~contacts/share/:ship/:group"
            render={(props) => {
              let groupPath = `/${props.match.params.ship}/${props.match.params.group}`;
              let thisContactPath = `/${props.match.params.ship}/${props.match.params.group}/${window.ship}`
              let contactList = state.contacts;
              contactList = (contactList !== undefined)
                ? state.contacts[groupPath]
                : {};
              let contact = (contactList !== undefined)
                ? contactList[window.ship]
                : {};

              let defaultList = (contactList !== undefined)
              ? contactList["/~/default"]
              : {};
              
              let rootIdentity = (contactList !== undefined)
              ? defaultList[window.ship]
              : {};

              return (
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected={groupPath}>
                  <Contacts
                    activeDrawer="rightPanel"
                    contacts={contactList}
                    path={groupPath}
                    selectedContact={thisContactPath} />
                  <ContactCard
                    contact={contact}
                    path={groupPath}
                    ship={window.ship}
                    share={true}
                    rootIdentity={rootIdentity}
                  />
                </Skeleton>
              )
            }}
          />
            <Route exact path="/~contacts/me"
            render={ (props) => {
              let contactList = state.contacts["/~/default"];
              let me = (contactList !== undefined) 
              ? contactList[window.ship]
              : {};
              return(
                <Skeleton
                  spinner={state.spinner}
                  contacts={state.contacts}
                  activeDrawer="rightPanel"
                  selected="me">
                    <ContactCard
                    path="/~/default"
                    contact={me}
                    ship={window.ship}
                    />
                  </Skeleton>
              )
            }}
            />
        </div>
      </BrowserRouter>
    )
  }
}

