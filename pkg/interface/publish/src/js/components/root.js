import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import { store } from '/store';
import { api } from '/api';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/lib/new';
import { JoinScreen } from '/components/lib/join';
import { Notebook } from '/components/lib/notebook';
import { Note } from '/components/lib/note';
import { NewPost } from '/components/lib/new-post';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    const { props, state } = this;

    let contacts = !!state.contacts ? state.contacts : {};

    return (
      <BrowserRouter>
        <Route exact path="/~publish"
          render={ (props) => {
            return (
              <Skeleton
              popout={false}
              active={"sidebar"}
              rightPanelHide={true}
              sidebarShown={true}
              spinner={state.spinner}
              invites={state.invites}
              notebooks={state.notebooks}
              contacts={contacts}>
                <div className={`h-100 w-100 overflow-x-hidden flex flex-column
                 bg-white bg-gray0-d dn db-ns`}>
                  <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                    <p className="f9 pt3 gray2 w-100 h-100 dtc v-mid tc">
                      Select or create a notebook to begin.
                    </p>
                  </div>
                </div>
              </Skeleton>
            )
          }}
        />
      <Route exact path="/~publish/new"
        render={ (props) => {
          return (
            <Skeleton
            popout={false}
            active={"rightPanel"}
            rightPanelHide={false}
            sidebarShown={state.sidebarShown}
            spinner={state.spinner}
            invites={state.invites}
            notebooks={state.notebooks}
            contacts={contacts}>
              <NewScreen
                notebooks={state.notebooks}
                groups={state.groups}
                api={api}
                {...props}
              />
            </Skeleton>
          )
        }}/>
      <Route exact path="/~publish/join"
              render={ (props) => {
                return (
                  <Skeleton
                  popout={false}
                  active={"rightPanel"}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  spinner={state.spinner}
                  invites={state.invites}
                  notebooks={state.notebooks}
                  contacts={contacts}>
                    <JoinScreen notebooks={state.notebooks} {...props} />
                  </Skeleton>
                )
              }}/>
      <Route exact path="/~publish/:popout?/notebook/:ship/:notebook/:view?"
        render={ (props) => {
          let view = (props.match.params.view)
          ? props.match.params.view
          : "posts";

          let popout = !!props.match.params.popout || false;

          let ship = props.match.params.ship || "";
          let notebook = props.match.params.notebook || "";

          let path = `${ship}/${notebook}`;

          let bookGroupPath =
          state.notebooks[ship][notebook]["subscribers-group-path"];
          let notebookContacts = (bookGroupPath in contacts)
            ? contacts[bookGroupPath] : {};

          if (view === "new") {
            return (
              <Skeleton
                popout={popout}
                active={"rightPanel"}
                rightPanelHide={false}
                sidebarShown={state.sidebarShown}
                spinner={state.spinner}
                invites={state.invites}
                notebooks={state.notebooks}
                contacts={contacts}
                path={path}>
                <NewPost
                  notebooks={state.notebooks}
                  ship={ship}
                  book={notebook}
                  sidebarShown={state.sidebarShown}
                  popout={popout}
                  {...props}
                />
              </Skeleton>
            );
          }
          else {
            return (
              <Skeleton
                popout={popout}
                active={"rightPanel"}
                rightPanelHide={false}
                sidebarShown={state.sidebarShown}
                spinner={state.spinner}
                invites={state.invites}
                notebooks={state.notebooks}
                contacts={contacts}
                path={path}>
                <Notebook
                  notebooks={state.notebooks}
                  view={view}
                  ship={ship}
                  book={notebook}
                  groups={state.groups}
                  contacts={notebookContacts}
                  sidebarShown={state.sidebarShown}
                  popout={popout}
                  permissions={state.permissions}
                  {...props}
                />
              </Skeleton>
            );
          }
        }}/>
      <Route exact path="/~publish/:popout?/note/:ship/:notebook/:note"
        render={ (props) => {
          let ship = props.match.params.ship || "";
          let notebook = props.match.params.notebook || "";
          let path = `${ship}/${notebook}`
          let note = props.match.params.note || "";

          let popout = !!props.match.params.popout || false;

          let bookGroupPath =
            state.notebooks[ship][notebook]["subscribers-group-path"];
          let notebookContacts = (bookGroupPath in state.contacts)
            ? contacts[bookGroupPath] : {};

          return (
            <Skeleton
              popout={popout}
              active={"rightPanel"}
              rightPanelHide={false}
              sidebarShown={state.sidebarShown}
              spinner={state.spinner}
              invites={state.invites}
              notebooks={state.notebooks}
              contacts={contacts}
              path={path}>
              <Note
                notebooks={state.notebooks}
                book={notebook}
                contacts={notebookContacts}
                ship={ship}
                note={note}
                sidebarShown={state.sidebarShown}
                popout={popout}
                {...props}
              />
            </Skeleton>
          );
        }}/>
      </BrowserRouter>
    )
  }
}

export default Root
