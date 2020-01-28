import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/lib/new';
import { JoinScreen } from '/components/lib/join';
import { Notebook } from '/components/lib/notebook';
import { Note } from '/components/lib/note';

//TODO add new note route
export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    const { props, state } = this;

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
              notebooks={state.notebooks}>
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
            sidebarShown={true}
            notebooks={state.notebooks}>
              <NewScreen
              groups={state.groups}/>
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
                  sidebarShown={true}
                  notebooks={state.notebooks}>
                    <JoinScreen/>
                  </Skeleton>
                )
              }}/>
      <Route exact path="/~publish/(popout)?/notebook/:ship/:notebook/:view?"
        render={ (props) => {
          let view = (props.match.params.view)
          ? props.match.params.view
          : "posts";

          let ship = props.match.params.ship || "";
          let notebook = props.match.params.notebook || "";

          return (
            <Skeleton
            popout={false}
            active={"rightPanel"}
            rightPanelHide={false}
            sidebarShown={true}
            notebooks={state.notebooks}>
              <Notebook
              notebooks={state.notebooks}
              view={view}
              ship={ship}
              book={notebook}/>
            </Skeleton>
          )
        }}/>
      <Route exact path="/~publish/(popout)?/note/:ship/:notebook/:note"
        render={ (props) => {
          let ship = props.match.params.ship || "";
          let notebook = props.match.params.notebook || "";
          let note = props.match.params.note || "";

          return (
            <Skeleton
            popout={false}
            active={"rightPanel"}
            rightPanelHide={false}
            sidebarShown={true}
            notebooks={state.notebooks}>
              <Note
              notebooks={state.notebooks}
              book={notebook}
              ship={ship}
              note={note}/>
            </Skeleton>
          )
        }}/>
      </BrowserRouter>
    )
  }
}

export default Root
