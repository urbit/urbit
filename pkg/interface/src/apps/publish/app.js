import React from 'react';
import { Route, Switch } from 'react-router-dom';
import _ from 'lodash';

import './css/custom.css';

import PublishApi from '../../api/publish';
import PublishStore from '../../store/publish';
import PublishSubscription from '../../subscription/publish';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/lib/new';
import { JoinScreen } from './components/lib/join';
import { Notebook } from './components/lib/notebook';
import { Note } from './components/lib/note';
import { NewPost } from './components/lib/new-post';
import { EditPost } from './components/lib/edit-post';

export default class PublishApp extends React.Component {
  constructor(props) {
    super(props);
    this.store = new PublishStore();
    this.state = this.store.state;
    this.unreadTotal = 0;
    this.resetControllers();
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    document.title = 'OS1 - Publish';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.store.setStateHandler(this.setState.bind(this));

    const channel = new this.props.channel();
    this.api = new PublishApi(this.props.ship, channel, this.store);

    this.subscription = new PublishSubscription(this.store, this.api, channel);
    this.subscription.start();
    this.api.fetchNotebooks();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.store.setStateHandler(() => {});
    this.resetControllers();
  }

  render() {
    const { state, props } = this;

    const contacts = state.contacts ? state.contacts : {};
    const associations = state.associations ? state.associations : { contacts: {} };
    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

    const notebooks = state.notebooks ? state.notebooks : {};

    const unreadTotal = _.chain(notebooks)
      .values()
      .map(_.values)
      .flatten() // flatten into array of notebooks
      .filter((each) => {
        return ((selectedGroups.map((e) => {
          return e[0];
        }).includes(each?.['writers-group-path'])) ||
        (selectedGroups.length === 0));
      })
      .map('num-unread')
      .reduce((acc, count) => acc + count, 0)
      .value();

    if (this.unreadTotal !== unreadTotal) {
      document.title = unreadTotal > 0 ? `OS1 - Publish (${unreadTotal})` : 'OS1 - Publish';
      this.unreadTotal = unreadTotal;
    }

    return (
      <Switch>
        <Route exact path="/~publish"
          render={(props) => {
            return (
              <Skeleton
                popout={false}
                active={'sidebar'}
                rightPanelHide={true}
                sidebarShown={true}
                invites={state.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={this.api}
              >
                <div className={`h-100 w-100 overflow-x-hidden flex flex-column
                 bg-white bg-gray0-d dn db-ns`}
                >
                  <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                    <p className="f9 pt3 gray2 w-100 h-100 dtc v-mid tc">
                      Select or create a notebook to begin.
                    </p>
                  </div>
                </div>
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~publish/new"
          render={(props) => {
            return (
              <Skeleton
                popout={false}
                active={'rightPanel'}
                rightPanelHide={false}
                sidebarShown={state.sidebarShown}
                invites={state.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={this.api}
              >
                <NewScreen
                  associations={associations.contacts}
                  notebooks={notebooks}
                  groups={state.groups}
                  contacts={contacts}
                  api={this.api}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~publish/join/:ship?/:notebook?"
          render={(props) => {
            const ship = props.match.params.ship || '';
            const notebook = props.match.params.notebook || '';
            return (
              <Skeleton
                popout={false}
                active={'rightPanel'}
                rightPanelHide={false}
                sidebarShown={state.sidebarShown}
                invites={state.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={this.api}
              >
                <JoinScreen
                  notebooks={notebooks}
                  ship={ship}
                  notebook={notebook}
                  api={this.api}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~publish/:popout?/notebook/:ship/:notebook/:view?"
          render={(props) => {
            const view = (props.match.params.view)
              ? props.match.params.view
              : 'posts';

            const popout = Boolean(props.match.params.popout) || false;

            const ship = props.match.params.ship || '';
            const notebook = props.match.params.notebook || '';

            const path = `${ship}/${notebook}`;

            const bookGroupPath =
              notebooks?.[ship]?.[notebook]?.['subscribers-group-path'];

            const notebookContacts = (bookGroupPath in contacts)
              ? contacts[bookGroupPath] : {};

            if (view === 'new') {
              return (
                <Skeleton
                  popout={popout}
                  active={'rightPanel'}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  invites={state.invites}
                  notebooks={notebooks}
                  associations={associations}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  path={path}
                  api={this.api}
                >
                  <NewPost
                    notebooks={notebooks}
                    ship={ship}
                    book={notebook}
                    sidebarShown={state.sidebarShown}
                    popout={popout}
                    api={this.api}
                    {...props}
                  />
                </Skeleton>
              );
            } else {
              return (
                <Skeleton
                  popout={popout}
                  active={'rightPanel'}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  invites={state.invites}
                  notebooks={notebooks}
                  associations={associations}
                  contacts={contacts}
                  selectedGroups={selectedGroups}
                  path={path}
                  api={this.api}
                >
                  <Notebook
                    notebooks={notebooks}
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
                    api={this.api}
                    {...props}
                  />
                </Skeleton>
              );
            }
          }}
        />
        <Route exact path="/~publish/:popout?/note/:ship/:notebook/:note/:edit?"
          render={(props) => {
            const ship = props.match.params.ship || '';
            const notebook = props.match.params.notebook || '';
            const path = `${ship}/${notebook}`;
            const note = props.match.params.note || '';

            const popout = Boolean(props.match.params.popout) || false;

            const bookGroupPath =
              notebooks?.[ship]?.[notebook]?.['subscribers-group-path'];
            const notebookContacts = (bookGroupPath in state.contacts)
              ? contacts[bookGroupPath] : {};

            const edit = Boolean(props.match.params.edit) || false;

            if (edit) {
              return (
                <Skeleton
                  popout={popout}
                  active={'rightPanel'}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  invites={state.invites}
                  notebooks={notebooks}
                  selectedGroups={selectedGroups}
                  associations={associations}
                  contacts={contacts}
                  path={path}
                  api={this.api}
                >
                  <EditPost
                    notebooks={notebooks}
                    book={notebook}
                    note={note}
                    ship={ship}
                    sidebarShown={state.sidebarShown}
                    popout={popout}
                    api={this.api}
                    {...props}
                  />
                </Skeleton>
              );
            } else {
              return (
                <Skeleton
                  popout={popout}
                  active={'rightPanel'}
                  rightPanelHide={false}
                  sidebarShown={state.sidebarShown}
                  invites={state.invites}
                  notebooks={notebooks}
                  associations={associations}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  path={path}
                  api={this.api}
                >
                  <Note
                    notebooks={notebooks}
                    book={notebook}
                    groups={state.groups}
                    contacts={notebookContacts}
                    ship={ship}
                    note={note}
                    sidebarShown={state.sidebarShown}
                    popout={popout}
                    api={this.api}
                    {...props}
                  />
                </Skeleton>
              );
            }
          }}
        />
      </Switch>
    );
  }
}
