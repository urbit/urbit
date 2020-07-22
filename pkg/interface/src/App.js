import { hot } from 'react-hot-loader/root';
import 'react-hot-loader';
import * as React from 'react';
import {useState} from 'react'
import { BrowserRouter as Router, Route, withRouter, Switch } from 'react-router-dom';
import styled, { ThemeProvider, createGlobalStyle } from 'styled-components';
import './css/indigo-static.css';
import './css/fonts.css';
import light from './themes/light';
import { Box, Text, Row, Icon, Rule } from '@tlon/indigo-react';

import LaunchApp from './apps/launch/app';
import ChatApp from './apps/chat/app';
import DojoApp from './apps/dojo/app';
import GroupsApp from './apps/groups/app';
import LinksApp from './apps/links/app';
import PublishApp from './apps/publish/app';

import StatusBar from './components/StatusBar';
import ModuleContext from './components/ModuleContext';
import NotFound from './components/404';

import GlobalStore from './store/store';
import GlobalSubscription from './subscription/global';
import GlobalApi from './api/global';

const Root = styled.div`
  font-family: ${p => p.theme.fonts.sans};
  height: 100%;
  width: 100%;
  padding: 0;
  margin: 0;
`;

// const CommandResult = ({icon, text, subtext, isSelected}) => {
//   if (isSelected === false) {
//     return (
//       <Row py='2' px='2'>
//         <Icon icon={icon}  mr='1' />
//         <Text mr='1'>{text}</Text>
//         <Text gray>{subtext}</Text>
//       </Row>
//     )
//   }
//   return (
//     <Row py='2' px='2' backgroundColor='blue'>
//       <Icon fill='white' icon={icon} mr='1' />
//       <Text color='white' mr='1'>{text}</Text>
//       <Text color='white'>{subtext}</Text>
//     </Row>
//   )
// }

const CommandResult = ({icon, text, subtext, isSelected}) => {
  const [isHovered, setHover] = useState(isSelected)
  
  return (
    <Row
      py='2'
      px='2'
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      backgroundColor={isHovered ? 'blue' : 'white'}
      >
        {
          isHovered
            ? <>
                <Icon fill='white' icon={icon} mr='1' />
                <Text color='white' mr='1'>{text}</Text>
                <Text color='white'>{subtext}</Text>
              </>
            : <>
                <Icon fill='black' icon={icon} mr='1' />
                <Text mr='1'>{text}</Text>
                <Text gray>{subtext}</Text>
              </>
        }

    </Row>
  )
}


const StatusBarWithRouter = withRouter(StatusBar);

class App extends React.Component {
  constructor(props) {
    super(props);
    this.ship = window.ship;
    this.store = new GlobalStore();
    this.store.setStateHandler(this.setState.bind(this));
    this.state = this.store.state;

    this.appChannel = new window.channel();
    this.api = new GlobalApi(this.ship, this.appChannel, this.store);
    this.subscription =
      new GlobalSubscription(this.store, this.api, this.appChannel);
  }

  componentDidMount() {
    this.subscription.start();
    this.api.local.getBaseHash();
  }

  render() {
    const channel = window.channel;

    const associations = this.state.associations ? this.state.associations : { contacts: {} };

    const selectedGroups = this.state.selectedGroups ? this.state.selectedGroups : [];

    const { state } = this;

    return (
      <ThemeProvider theme={light}>
        <Root>
          <Router>
            <StatusBarWithRouter props={this.props}
              associations={associations}
              invites={this.state.invites}
              api={this.api}
              connection={this.state.connection}
              subscription={this.subscription}
              />
            

            <Box backgroundColor='gray' width='100vw' height='100vh' position='absolute' top='0' right='0' zIndex='9'>
              <Row justifyContent='center'>
                <Box
                  borderRadius=''
                  mt='20vh'
                  width='600px'
                  borderRadius='2'
                  backgroundColor='white'>
                  <Row py='3' px='2'>
                    <Text>Input</Text>
                  </Row>
                  <Rule/>

                  <CommandResult 
                    icon='ChevronEast'
                    text='Join'
                    subtext='an existing collection, stream, or chat'
                    isSelected={true}
                  />

                  <CommandResult 
                    icon='ChevronEast'
                    text='Create'
                    subtext='a new collection, stream, or chat'
                    isSelected={false}
                  />

                  <CommandResult 
                    text='Publish'
                    subtext=''
                    isSelected={false}
                  />

                  <CommandResult 
                    text='Collections'
                    subtext=''
                    isSelected={false}
                  />

                  <CommandResult 
                    text='Chat'
                    subtext=''
                    isSelected={false}
                  />

                </Box>
              </Row>
            </Box>

            <ModuleContext>
            <Switch>
              <Route exact path="/"
              render={ p => (
                <LaunchApp
                  ship={this.ship}
                  api={this.api}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~chat" render={ p => (
                <ChatApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~dojo" render={ p => (
                <DojoApp
                  ship={this.ship}
                  channel={channel}
                  selectedGroups={selectedGroups}
                  subscription={this.subscription}
                  {...p}
                />
              )}
              />
              <Route path="/~groups" render={ p => (
                <GroupsApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~link" render={ p => (
                <LinksApp
                  ship={this.ship}
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route path="/~publish" render={ p => (
                <PublishApp
                  ship={this.ship}
                  api={this.api}
                  subscription={this.subscription}
                  {...state}
                  {...p}
                />
              )}
              />
              <Route component={NotFound} />
              </Switch>
            </ModuleContext>
          </Router>
        </Root>
      </ThemeProvider>
    );
  }
}

export default process.env.NODE_ENV === 'production' ? App : hot(App);

