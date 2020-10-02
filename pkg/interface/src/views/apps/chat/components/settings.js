import React, { Component, Fragment } from 'react';

import { deSig } from '~/logic/lib/util';
import { MetadataSettings } from '~/views/components/metadata/settings';
import { Spinner } from '~/views/components/Spinner';

import ChatHeader from './lib/ChatHeader';
import { DeleteButton } from './lib/delete-button';
import { GroupifyButton } from './lib/groupify-button';

import { Text, Col, Box } from '@tlon/indigo-react';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      awaiting: false,
      type: 'Editing chat...'
    };

    this.changeLoading = this.changeLoading.bind(this);
  }

  componentDidMount() {
    if (this.state.isLoading && (this.props.station in this.props.inbox)) {
      this.setState({ isLoading: false });
    }
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    if (state.isLoading && !(props.station in props.inbox)) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~chat');
      });
    } else if (state.isLoading && (props.station in props.inbox)) {
      this.setState({ isLoading: false });
    }
  }

  changeLoading(isLoading, awaiting, type, closure) {
    this.setState({
      isLoading,
      awaiting,
      type
    }, closure);
  }

  renderLoading() {
    return (
      <Spinner
        awaiting={this.state.awaiting}
        classes="absolute right-2 bottom-2 ba pa2 b--gray1-d"
        text={this.state.type}
      />
    );
  }

  renderNormal() {
    const {
      associations,
      association,
      contacts,
      groups,
      api,
      station,
      match,
      history
    } = this.props;
    const isOwner = deSig(match.params.ship) === window.ship;

    return (
      <Fragment>
        <Text display='block' pb='2' fontSize='1'>Chat Settings</Text>
        <GroupifyButton
          isOwner={isOwner}
          association={association}
          associations={associations}
          contacts={contacts}
          groups={groups}
          api={api}
          changeLoading={this.changeLoading} />
        <DeleteButton
          isOwner={isOwner}
          changeLoading={this.changeLoading}
          station={station}
          association={association}
          contacts={contacts}
          history={history}
          api={api} />
        <MetadataSettings
          isOwner={isOwner}
          changeLoading={this.changeLoading}
          api={api}
          association={association}
          resource="chat"
          app="chat"
          module=""
        />
        <Spinner
          awaiting={this.state.awaiting}
          classes="absolute right-2 bottom-2 ba pa2 b--gray1-d"
          text={this.state.type}
        />
      </Fragment>
    );
  }

  render() {
    const { state } = this;
    const {
      api,
      group,
      association,
      station,
      popout,
      sidebarShown,
      match,
      location
    } = this.props;

    return (
      <Col height='100%' width='100%' overflowX='hidden'>
        <ChatHeader
          match={match}
          location={location}
          api={api}
          group={group}
          association={association}
          station={station}
          sidebarShown={sidebarShown}
          popout={popout} />
        <Box width='100%' pl='3' mt='3'>
          {(state.isLoading) ? this.renderLoading() : this.renderNormal() }
        </Box>
      </Col>
    );
  }
}
