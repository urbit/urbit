import React, { Component } from 'react';
import { deSig, uxToHex, writeText } from '../../../lib/util';
import { Link } from 'react-router-dom';

import { Spinner } from './lib/icons/icon-spinner';
import { ChatTabBar } from './lib/chat-tabbar';
import { InviteSearch } from '../../../components/InviteSearch';
import SidebarSwitcher from './lib/icons/icon-sidebar-switch';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      title: '',
      description: '',
      color: '',
      // groupify settings
      targetGroup: null,
      inclusive: false,
      awaiting: false,
      type: 'Editing chat...'
    };

    this.renderDelete = this.renderDelete.bind(this);
    this.changeTargetGroup = this.changeTargetGroup.bind(this);
    this.changeInclusive = this.changeInclusive.bind(this);
    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
    this.changeColor = this.changeColor.bind(this);
    this.submitColor = this.submitColor.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.association && 'metadata' in props.association) {
      this.setState({
        title: props.association.metadata.title,
        description: props.association.metadata.description,
        color: `#${uxToHex(props.association.metadata.color)}`
      });
    }
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

    if ((state.title === '') && (prevProps !== props)) {
      if (props.association && 'metadata' in props.association)
        this.setState({
          title: props.association.metadata.title,
          description: props.association.metadata.description,
          color: `#${uxToHex(props.association.metadata.color)}`
        });
    }
  }

  changeTargetGroup(target) {
    if (target.groups.length === 1) {
      this.setState({ targetGroup: target.groups[0] });
    } else {
      this.setState({ targetGroup: null });
    }
  }

  changeInclusive(event) {
    this.setState({ inclusive: Boolean(event.target.checked) });
  }

  changeTitle() {
    this.setState({ title: event.target.value });
  }

  changeDescription() {
    this.setState({ description: event.target.value });
  }

  changeColor() {
    this.setState({ color: event.target.value });
  }

  submitColor() {
    const { props, state } = this;

    let color = state.color;
    if (color.startsWith('#')) {
      color = state.color.substr(1);
    }
    const hexExp = /([0-9A-Fa-f]{6})/;
    const hexTest = hexExp.exec(color);
    let currentColor = '000000';
    if (props.association && 'metadata' in props.association) {
      currentColor = uxToHex(props.association.metadata.color);
    }
    if (hexTest && (hexTest[1] !== currentColor)) {
      const chatOwner = (deSig(props.match.params.ship) === window.ship);
      const association =
        (props.association) && ('metadata' in props.association)
          ? props.association : {};

      if (chatOwner) {
        this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
          props.api.metadataAdd(
            association['app-path'],
            association['group-path'],
            association.metadata.title,
            association.metadata.description,
            association.metadata['date-created'],
            color
          ).then(() => {
            this.setState({ awaiting: false });
          });
        }));
      }
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
        props.api.chatView.delete(props.station);
    }));
  }

  groupifyChat() {
    const { props, state } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Converting chat...'
    }, (() => {
      props.api.chatView.groupify(
        props.station, state.targetGroup, state.inclusive
      ).then(() => this.setState({ awaiting: false }));
    }));
  }

  renderDelete() {
    const { props } = this;

    const chatOwner = (deSig(props.match.params.ship) === window.ship);

    const deleteButtonClasses = (chatOwner) ? 'b--red2 red2 pointer bg-gray0-d' : 'b--gray3 gray3 bg-gray0-d c-default';
    const leaveButtonClasses = (!chatOwner) ? 'pointer' : 'c-default';

    return (
      <div>
      <div className={'w-100 fl mt3 ' + ((chatOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Leave Chat</p>
        <p className="f9 gray2 db mb4">Remove this chat from your chat list. You will need to request for access again.</p>
        <a onClick={(!chatOwner) ? this.deleteChat.bind(this) : null}
           className={'dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d ' + leaveButtonClasses}
        >Leave this chat</a>
      </div>
        <div className={'w-100 fl mt3 ' + ((!chatOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Delete Chat</p>
          <p className="f9 gray2 db mb4">Permanently delete this chat. All current members will no longer see this chat.</p>
          <a onClick={(chatOwner) ? this.deleteChat.bind(this) : null}
           className={'dib f9 ba pa2 ' + deleteButtonClasses}
          >Delete this chat</a>
      </div>
      </div>
    );
  }

  renderGroupify() {
    const { props, state } = this;

    const chatOwner = (deSig(props.match.params.ship) === window.ship);

    const ownedUnmanagedVillage =
      chatOwner &&
      props.station.slice(0, 3) === '/~/' &&
      props.permission.kind === 'white';

    if (!ownedUnmanagedVillage) {
      return null;
    } else {
      let inclusiveToggle = <div />;
      if (state.targetGroup) {
        // TODO toggle component into /lib
        const inclusiveClasses = state.inclusive
          ? 'relative checked bg-green2 br3 h1 toggle v-mid z-0'
          : 'relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0';
        inclusiveToggle = (
          <div className="mt4">
            <input
              type="checkbox"
              style={{ WebkitAppearance: 'none', width: 28 }}
              className={inclusiveClasses}
              onChange={this.changeInclusive}
            />
            <span className="dib f9 white-d inter ml3">
              Add all members to group
            </span>
            <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
              Add chat members to the group if they aren't in it yet
            </p>
          </div>
        );
      }

      const groups = {};
      Object.keys(props.permissions).forEach((pem) => {
        groups[pem] = props.permissions[pem].who;
      });

      return (
        <div>
          <div className={'w-100 fl mt3'} style={{ maxWidth: '29rem' }}>
            <p className="f8 mt3 lh-copy db">Convert Chat</p>
            <p className="f9 gray2 db mb4">
              Convert this chat into a group with associated chat, or select a
              group to add this chat to.
            </p>
            <InviteSearch
              groups={groups}
              contacts={props.contacts}
              associations={props.associations}
              groupResults={true}
              shipResults={false}
              invites={{
                groups: state.targetGroup ? [state.targetGroup] : [],
                ships: []
              }}
              setInvite={this.changeTargetGroup}
            />
            {inclusiveToggle}
            <a onClick={this.groupifyChat.bind(this)}
               className={'dib f9 black gray4-d bg-gray0-d ba pa2 mt4 b--black b--gray1-d pointer'}
            >
              Convert to group
            </a>
          </div>
        </div>
      );
    }
  }

  renderMetadataSettings() {
    const { props, state } = this;

    const chatOwner = (deSig(props.match.params.ship) === window.ship);

    const association = (props.association) && ('metadata' in props.association)
      ? props.association : {};

    return(
      <div>
        <div className={'w-100 pb6 fl mt3 ' + ((chatOwner) ? '' : 'o-30')}>
        <p className="f8 mt3 lh-copy">Rename</p>
        <p className="f9 gray2 db mb4">Change the name of this chat</p>
        <div className="relative w-100 flex"
        style={{ maxWidth: '29rem' }}
        >
          <input
            className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
            'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
            value={this.state.title}
            disabled={!chatOwner}
            onChange={this.changeTitle}
            onBlur={() => {
              if (chatOwner) {
                this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
                  props.api.metadataAdd(
                    association['app-path'],
                    association['group-path'],
                    this.state.title,
                    association.metadata.description,
                    association.metadata['date-created'],
                    uxToHex(association.metadata.color)
                  ).then(() => {
                    this.setState({ awaiting: false });
                  });
                }));
              }
            }}
          />
          </div>
          <p className="f8 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 db mb4">Change the description of this chat</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: '29rem' }}
          >
            <input
              className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
                'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.description}
              disabled={!chatOwner}
              onChange={this.changeDescription}
              onBlur={() => {
                if (chatOwner) {
                  this.setState({ awaiting: true, type: 'Editing chat...' }, (() => {
                    props.api.metadataAdd(
                      association['app-path'],
                      association['group-path'],
                      association.metadata.title,
                      this.state.description,
                      association.metadata['date-created'],
                      uxToHex(association.metadata.color)
                    ).then(() => {
                      this.setState({ awaiting: false });
                    });
                  }));
                }
              }}
            />
          </div>
          <p className="f8 mt3 lh-copy">Change color</p>
          <p className="f9 gray2 db mb4">Give this chat a color when viewing group channels</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: '10rem' }}
          >
            <div className="absolute"
              style={{
                height: 16,
                width: 16,
                backgroundColor: state.color,
                top: 13,
                left: 11
                }}
            />
            <input
              className={'pl7 f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
                'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.color}
              disabled={!chatOwner}
              onChange={this.changeColor}
              onBlur={this.submitColor}
            />
          </div>
        </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;
    const isinPopout = this.props.popout ? 'popout/' : '';

    const permission = Array.from(props.permission.who.values());

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
              numPeers={permission.length}
              host={props.match.params.ship}
              api={props.api}
            />
          </div>
          <div className="w-100 pl3 mt4 cf">
            <Spinner awaiting={this.state.awaiting} classes="absolute right-2 bottom-2 ba pa2 b--gray1-d" text={this.state.type} />
          </div>
        </div>
      );
    }

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
          className="pl4 pt2 bb b--gray4 b--gray1-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
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
            numPeers={permission.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
          />
        </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Chat Settings</h2>
          <div className="w-100 mt3">
            <p className="f8 mt3 lh-copy">Share</p>
            <p className="f9 gray2 mb4">Share a shortcode to join this chat</p>
            <div className="relative w-100 flex"
              style={{ maxWidth: '29rem' }}
            >
              <input
                className="f8 mono ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 flex-auto mr3"
                disabled={true}
                value={props.station.substr(1)}
              />
              <span className="f8 pointer absolute pa3 inter"
                style={{ right: 12, top: 1 }}
                ref="copy"
                onClick={() => {
                  writeText(props.station.substr(1));
                  this.refs.copy.innerText = 'Copied';
                }}
              >
                Copy
              </span>
            </div>
          </div>
          {this.renderGroupify()}
          {this.renderDelete()}
          {this.renderMetadataSettings()}
          <Spinner awaiting={this.state.awaiting} classes="absolute right-2 bottom-2 ba pa2 b--gray1-d" text={this.state.type} />
        </div>
      </div>
    );
  }
}
