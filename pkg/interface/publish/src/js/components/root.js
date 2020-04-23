import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import _ from 'lodash';
import { store } from '/store';
import { api } from '/api';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/lib/new';
import { JoinScreen } from '/components/lib/join';
import { Notebook } from '/components/lib/notebook';
import { Note } from '/components/lib/note';
import { NewPost } from '/components/lib/new-post';
import { EditPost } from '/components/lib/edit-post';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.unreadTotal = 0;
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    //preload spinner asset
    new Image().src = "/~publish/Spinner.png";
  }

  render() {
    const { props, state } = this;

    let contacts = !!state.contacts ? state.contacts : {};
    let associations = !!state.associations ? state.associations : {contacts: {}}
    let selectedGroups = !!state.selectedGroups ? state.selectedGroups : [];

    const unreadTotal = _.chain(state.notebooks)
                         .values()
                         .map(_.values)
                         .flatten() // flatten into array of notebooks
                         .map('num-unread')
                         .reduce((acc, count) => acc + count, 0)
                         .value();

    if(this.unreadTotal !== unreadTotal) {
      document.title = unreadTotal > 0 ? `Publish - (${unreadTotal})` : 'Publish';
      this.unreadTotal = unreadTotal;
    }

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
              invites={state.invites}
              notebooks={state.notebooks}
              associations={associations}
              selectedGroups={selectedGroups}
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
            invites={state.invites}
            notebooks={state.notebooks}
            associations={associations}
            selectedGroups={selectedGroups}
            contacts={contacts}>
              <NewScreen
                associations={associations.contacts}
                notebooks={state.notebooks}
                groups={state.groups}
                contacts={contacts}
                api={api}
                {...props}
              />
            </Skeleton>
          )
        }}/>
      <Route exact path="/~publish/join/:ship?/:notebook?"
              render={ (props) => {
                let ship = props.match.params.ship || "";
                let notebook = props.match.params.notebook || "";
                return (
                  <Skeleton
                  popout={false}
                  active={"rightPanel"}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  invites={state.invites}
                  notebooks={state.notebooks}
                  associations={associations}
                  selectedGroups={selectedGroups}
                  contacts={contacts}>
                    <JoinScreen
                    notebooks={state.notebooks}
                    ship={ship}
                    notebook={notebook}
                    {...props} />
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
                invites={state.invites}
                notebooks={state.notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
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
                invites={state.invites}
                notebooks={state.notebooks}
                associations={associations}
                contacts={contacts}
                selectedGroups={selectedGroups}
                path={path}>
                <Notebook
                  notebooks={state.notebooks}
                  view={view}
                  ship={ship}
                  book={notebook}
                  groups={state.groups}
                  contacts={contacts}
                  notebookContacts={notebookContacts}
                  associations={associations.contacts}
                  sidebarShown={state.sidebarShown}
                  popout={popout}
                  permissions={state.permissions}
                  {...props}
                />
              </Skeleton>
            );
          }
        }}/>
      <Route exact path="/~publish/:popout?/note/:ship/:notebook/:note/:edit?"
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

          let edit = !!props.match.params.edit || false;

          if (edit) {
            return (
              <Skeleton
              popout={popout}
              active={"rightPanel"}
              rightPanelHide={false}
              sidebarShown={state.sidebarShown}
              invites={state.invites}
              notebooks={state.notebooks}
              selectedGroups={selectedGroups}
              associations={associations}
              contacts={contacts}
              path={path}>
              <EditPost
                notebooks={state.notebooks}
                book={notebook}
                note={note}
                ship={ship}
                sidebarShown={state.sidebarShown}
                popout={popout}
                {...props}/>
              </Skeleton>
            )
          }
          else {
            return (
              <Skeleton
                popout={popout}
                active={"rightPanel"}
                rightPanelHide={false}
                sidebarShown={state.sidebarShown}
                invites={state.invites}
                notebooks={state.notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                path={path}>
                <Note
                  notebooks={state.notebooks}
                  book={notebook}
                  groups={state.groups}
                  contacts={notebookContacts}
                  ship={ship}
                  note={note}
                  sidebarShown={state.sidebarShown}
                  popout={popout}
                  {...props}
                />
              </Skeleton>
            );
          }
        }}/>
      </BrowserRouter>
    )
  }
}

export default Root;
