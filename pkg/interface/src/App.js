import * as React from 'react';
import { BrowserRouter as Router, Route, Link, withRouter } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import './css/indigo-static.css';
import './css/fonts.css';
import { light } from '@tlon/indigo-react';

import ChatApp from './apps/chat/ChatApp';
import DojoApp from './apps/dojo/DojoApp';
import StatusBar from './components/StatusBar';
import GroupsApp from './apps/groups/GroupsApp';
import LinksApp from './apps/links/LinksApp';
import PublishApp from './apps/publish/PublishApp';

import Store from './store';
import Subscription from './subscription';
import Api from './api';

// const Style = createGlobalStyle`
//   ${cssReset}
//   html {
//     background-color: ${p => p.theme.colors.white};
//   }
//
//   strong {
//     font-weight: 600;
//   }
// `;

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  line-height: ${p => p.theme.lineHeights.regular};
  max-height: 100vh;
  min-height: 100vh;
`;

const Home = () => (
  <div>
    Home
    <Link className="db" to='/~chat'>Chat</Link>
    <Link className="db" to='/~dojo'>Dojo</Link>
    <Link className="db" to='/~groups'>Groups</Link>
    <Link className="db" to='/~link'>Links</Link>
    <Link className="db" to='~publish'>Publish</Link>
  </div>
);

const StatusBarWithRouter = withRouter(StatusBar);

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));
    this.state = this.store.state;

    this.appChannel = new window.channel();
    this.api = new Api(this.ship, this.appChannel, this.store);
  }

  componentDidMount() {
    this.subscription = new Subscription(this.store, this.api, this.appChannel);
    this.subscription.start();
  }

  render() {
    const channel = window.channel;

    const associations = this.state.associations ? this.state.associations : { contacts: {} };
    const selectedGroups = this.state.selectedGroups ? this.state.selectedGroups : [];

    return (
      <ThemeProvider theme={light}>
        <Root>
          <Router>
            <StatusBarWithRouter props={this.props}
            associations={associations}
            invites={this.state.invites}
            api={this.api}
            />
            <div>
              <Route exact path="/" component={Home} />
              <Route path="/~chat" render={
                p => <ChatApp ship={this.ship} channel={channel} {...p} selectedGroups={selectedGroups} />
              }
              />
              <Route path="/~dojo" render={
                p => <DojoApp ship={this.ship} channel={channel} {...p} selectedGroups={selectedGroups} />
              }
              />
              <Route path="/~groups" render={
                p => <GroupsApp ship={this.ship} channel={channel} {...p} selectedGroups={selectedGroups} />
              }
              />
              <Route path="/~link" render={
                p => <LinksApp ship={this.ship} channel={channel} {...p} selectedGroups={selectedGroups} />
              }
              />
              <Route path="/~publish" render={
                p => <PublishApp ship={this.ship} channel={channel} {...p} selectedGroups={selectedGroups} />
              }
              />
            </div>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}
