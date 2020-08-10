import React, { Component } from 'react';
import { deSig, uxToHex, writeText } from '../../../lib/util';
import { Link } from 'react-router-dom';

import { MetadataColor } from './lib/metadata-color';
import { MetadataInput } from './lib/metadata-input';
import { DeleteButton } from './lib/delete-button';
import { GroupifyButton } from './lib/groupify-button';
import { Spinner } from '../../../components/Spinner';
import { ChatTabBar } from './lib/chat-tabbar';
import { InviteSearch } from '../../../components/InviteSearch';
import SidebarSwitcher from '../../../components/SidebarSwitch';
import Toggle from '../../../components/toggle';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      awaiting: false,
      type: 'Editing chat...'
    };
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    if (Boolean(state.isLoading) && !(props.station in props.inbox)) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~chat');
      });
    }
  }

  deleteChat() {
    const { props } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: (deSig(props.match.params.ship) === window.ship)
        ? 'Deleting chat...'
        : 'Leaving chat...'
    }, (() => {
        props.api.chat.delete(props.station);
    }));
  }

  groupifyChat(targetGroup, inclusive) {
    const { props, state } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Converting chat...'
    }, (() => {
      props.api.chat.groupify(
        props.station, targetGroup, inclusive
      ).then(() => this.setState({ awaiting: false }));
    }));
  }

  renderMembers() {
    const chatOwner = (deSig(props.match.params.ship) === window.ship);

    if (chatOwner) {
      return (
        <div className={'dib pt2 f9 pl6 lh-solid'}>
          <Link
            className={'no-underline ' + memColor}
            to={"/~groups/ship/:ship/:group/"}>
            Members
          </Link>
        </div>
      );
    }
    return (
      <div className="dib" style={{ width: 0 }}></div>
    );
  }

  renderMetadataSettings() {
    const { props, state } = this;

    const isOwner = (deSig(props.match.params.ship) === window.ship);

    const association = (props.association) && ('metadata' in props.association)
      ? props.association : {};

    return (
      <div>
        <MetadataInput
          title={'Rename'}
          description={'Change the name of this chat'}
          isDisabled={!isOwner}
          setValue={() => {
            this.setState({ awaiting: true, type: 'Editing chat...' }, () => {
              props.api.metadata.metadataAdd(
                'chat',
                association['app-path'],
                association['group-path'],
                state.title,
                association.metadata.description,
                association.metadata['date-created'],
                uxToHex(association.metadata.color)
              ).then(() => {
                this.setState({ awaiting: false });
              });
            });
          }} />
          <MetadataInput
            title={'Change description'}
            description={'Change the description of this chat'}
            isDisabled={!isOwner}
            setValue={() => {
              this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
                props.api.metadata.metadataAdd(
                  'chat',
                  association['app-path'],
                  association['group-path'],
                  association.metadata.title,
                  state.description,
                  association.metadata['date-created'],
                  uxToHex(association.metadata.color)
                ).then(() => {
                  this.setState({ awaiting: false });
                });
              }));
            }} />
        <MetadataColor initialValue={props.association.metadata.color} />
      </div>
    );
  }

  render() {
    const { props, state } = this;
    const isinPopout = this.props.popout ? 'popout/' : '';

    if (state.isLoading) {
      let title = props.station.substr(1);

      if ((props.association) && ('metadata' in props.association)) {
        title = (props.association.metadata.title !== '')
          ? props.association.metadata.title : props.station.substr(1);
      }

      return (
        <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
          <div
            className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
            style={{ height: '1rem' }}
          >
            <Link to="/~chat/">{'⟵ All Chats'}</Link>
          </div>
          <div
            className="pl4 pt2 bb b--gray4 b--gray2-d bg-gray0-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
            style={{ height: 48 }}
          >
            <SidebarSwitcher
              sidebarShown={this.props.sidebarShown}
              popout={this.props.popout}
            />
            <Link to={'/~chat/' + isinPopout + 'room' + props.station}
            className="pt2 white-d"
            >
              <h2
                className={'dib f9 fw4 lh-solid v-top ' +
                  ((title === props.station.substr(1)) ? 'mono' : '')}
                style={{ width: 'max-content' }}
              >
                {title}
              </h2>
            </Link>
            <ChatTabBar
              {...props}
              station={props.station}
              host={props.match.params.ship}
              api={props.api}
            />
          </div>
          <div className="w-100 pl3 mt4 cf">
            <Spinner awaiting={state.awaiting}
                     classes="absolute right-2 bottom-2 ba pa2 b--gray1-d"
                     text={state.type} />
          </div>
        </div>
      );
    }

    let title = props.station.substr(1);

    if ((props.association) && ('metadata' in props.association)) {
      title = (props.association.metadata.title !== '')
        ? props.association.metadata.title : props.station.substr(1);
    }

    const isOwner = deSig(props.match.params.ship) === window.ship;

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: '1rem' }}
        >
          <Link to="/~chat/">{'⟵ All Chats'}</Link>
        </div>
        <div
          className="pl4 pt2 bb b--gray4 b--gray1-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
            api={this.props.api}
          />
          <Link to={'/~chat/' + isinPopout + 'room' + props.station}
          className="pt2"
          >
            <h2
              className={'dib f9 fw4 lh-solid v-top ' +
                ((title === props.station.substr(1)) ? 'mono' : '')}
              style={{ width: 'max-content' }}
            >
              {title}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            isOwner={isOwner}
            popout={this.props.popout}
          />
        </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Chat Settings</h2>
          {this.renderMembers()}
          <GroupifyButton
            isOwner={isOwner}
            association={association}
            contacts={contacts}
            groups={groups}
            groupifyChat={this.groupifyChat.bind(this)}
            changeTargetGroup={this.changeTargetGroup.bind(this)}
            changeInclusive={this.changeInclusive.bind(this)}
          />
          <DeleteButton
            isOwner={isOwner}
            deleteChat={this.deleteChat.bind(this)} />
          {this.renderMetadataSettings()}
          <Spinner awaiting={state.awaiting}
                   classes="absolute right-2 bottom-2 ba pa2 b--gray1-d"
                   text={state.type} />
        </div>
      </div>
    );
  }
}
