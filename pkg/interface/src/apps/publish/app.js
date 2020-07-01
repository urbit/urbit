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
    this.unreadTotal = 0;
  }

  componentDidMount() {
    document.title = 'OS1 - Publish';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('publish');

    this.props.api.publish.fetchNotebooks();

    if (!this.props.sidebarShown) {
      this.props.api.local.sidebarToggle();
    }

  }

  componentWillUnmount() {
    this.props.subscription.stopApp('publish');
  }

  render() {
    const { props } = this;

    const contacts = props.contacts ? props.contacts : {};
    const associations = props.associations ? props.associations : { contacts: {} };
    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

    const notebooks = props.notebooks ? props.notebooks : {};

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

    const { api, groups, permissions, sidebarShown } = props;

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
                invites={props.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={api}
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
                sidebarShown={sidebarShown}
                invites={props.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={api}
              >
                <NewScreen
                  associations={associations.contacts}
                  notebooks={notebooks}
                  groups={groups}
                  contacts={contacts}
                  api={api}
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
                sidebarShown={sidebarShown}
                invites={props.invites}
                notebooks={notebooks}
                associations={associations}
                selectedGroups={selectedGroups}
                contacts={contacts}
                api={api}
              >
                <JoinScreen
                  notebooks={notebooks}
                  ship={ship}
                  notebook={notebook}
                  api={api}
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
                  sidebarShown={sidebarShown}
                  invites={props.invites}
                  notebooks={notebooks}
                  associations={associations}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  path={path}
                  api={api}
                >
                  <NewPost
                    notebooks={notebooks}
                    ship={ship}
                    book={notebook}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
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
                  sidebarShown={sidebarShown}
                  invites={props.invites}
                  notebooks={notebooks}
                  associations={associations}
                  contacts={contacts}
                  selectedGroups={selectedGroups}
                  path={path}
                  api={api}
                >
                  <Notebook
                    notebooks={notebooks}
                    view={view}
                    ship={ship}
                    book={notebook}
                    groups={groups}
                    contacts={contacts}
                    notebookContacts={notebookContacts}
                    associations={associations.contacts}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    permissions={permissions}
                    api={api}
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
            const notebookContacts = (bookGroupPath in contacts)
              ? contacts[bookGroupPath] : {};

            const edit = Boolean(props.match.params.edit) || false;

            if (edit) {
              return (
                <Skeleton
                  popout={popout}
                  active={'rightPanel'}
                  rightPanelHide={false}
                  sidebarShown={sidebarShown}
                  invites={props.invites}
                  notebooks={notebooks}
                  selectedGroups={selectedGroups}
                  associations={associations}
                  contacts={contacts}
                  path={path}
                  api={api}
                >
                  <EditPost
                    notebooks={notebooks}
                    book={notebook}
                    note={note}
                    ship={ship}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
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
                  sidebarShown={sidebarShown}
                  invites={props.invites}
                  notebooks={notebooks}
                  associations={associations}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  path={path}
                  api={api}
                >
                  <Note
                    notebooks={notebooks}
                    book={notebook}
                    groups={groups}
                    contacts={notebookContacts}
                    ship={ship}
                    note={note}
                    sidebarShown={sidebarShown}
                    popout={popout}
                    api={api}
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
