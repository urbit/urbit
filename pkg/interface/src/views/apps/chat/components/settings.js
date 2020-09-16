import React, { Component, Fragment } from 'react';

import { deSig } from '~/logic/lib/util';
import { MetadataSettings } from '~/views/components/metadata/settings';
import { Spinner } from '~/views/components/Spinner';

import ChatHeader from './lib/ChatHeader';
import { DeleteButton } from './lib/delete-button';
import { GroupifyButton } from './lib/groupify-button';

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
    const { state } = this;
    const {
      associations,
      association,
      contacts,
      groups,
      api,
      station,
      match
    } = this.props;
    const isOwner = deSig(match.params.ship) === window.ship;

    return (
      <Fragment>
        <h2 className="f8 pb2">Chat Settings</h2>
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
          api={api} />
        <MetadataSettings
          isOwner={isOwner}
          changeLoading={this.changeLoading}
          api={api}
          association={association}
          resource="chat"
          app="chat"
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

    const isInPopout = popout ? "popout/" : "";
    const title =
      ( association &&
        ('metadata' in association) &&
        (association.metadata.title !== '')
      ) ? association.metadata.title : station.substr(1);

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <ChatHeader
          match={match}
          location={location}
          api={api}
          group={group}
          association={association}
          station={station}
          sidebarShown={sidebarShown}
          popout={popout} />
        <div className="w-100 pl3 mt4 cf">
          {(state.isLoading) ? this.renderLoading() : this.renderNormal() }
        </div>
      </div>
    );
  }
}
