import React, { Component } from 'react';
import { Box, Row, Icon, Text } from '@tlon/indigo-react';
import defaultApps from '~/logic/lib/default-apps';
import Sigil from '~/logic/lib/sigil';
import { uxToHex, cite } from '~/logic/lib/util';
import withState from '~/logic/lib/withState';
import useHarkState from '~/logic/state/hark';
import useContactState from '~/logic/state/contact';
import useInviteState from '~/logic/state/invite';

export class OmniboxResult extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isSelected: false,
      hovered: false
    };
    this.setHover = this.setHover.bind(this);
    this.result = React.createRef();
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    if (prevProps &&
      !state.hovered &&
      prevProps.selected !== props.selected &&
      props.selected === props.link
      ) {
        this.result.current.scrollIntoView({ block: 'nearest' });
      }
  }

  getIcon(icon, selected, link, invites, notifications, text, color) {
    const iconFill = (this.state.hovered || (selected === link)) ? 'white' : 'black';
    const bulletFill = (this.state.hovered || (selected === link)) ? 'white' : 'blue';

    const inviteCount = [].concat(...Object.values(invites).map(obj => Object.values(obj)));

    let graphic = <div />;
    if (defaultApps.includes(icon.toLowerCase())
        || icon.toLowerCase() === 'links'
        || icon.toLowerCase() === 'terminal')
    {
      icon = (icon === 'Link')     ? 'Collection' :
             (icon === 'Terminal') ? 'Dojo'  : icon;
      graphic = <Icon display="inline-block" verticalAlign="middle" icon={icon} mr='2' size='18px' color={iconFill} />;
    } else if (icon === 'inbox') {
      graphic = <Box display='flex' verticalAlign='middle' position="relative">
        <Icon display='inline-block' verticalAlign='middle' icon='Inbox' mr='2' size='18px' color={iconFill} />
        {(notifications > 0 || inviteCount.length > 0) && (
          <Icon display='inline-block' icon='Bullet' style={{ position: 'absolute', top: -5, left: 5 }} color={bulletFill} />
        )}
      </Box>;
    } else if (icon === 'logout') {
      graphic = <Icon display="inline-block" verticalAlign="middle" icon='SignOut' mr='2' size='18px' color={iconFill} />;
    } else if (icon === 'profile') {
      text = text.startsWith('Profile') ? window.ship : text;
      graphic = <Sigil color={color} classes='dib flex-shrink-0 v-mid mr2' ship={text} size={18} icon padding={2} />;
    } else if (icon === 'home') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Home' mr='2' size='18px' color={iconFill} />;
    } else if (icon === 'notifications') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Inbox' mr='2' size='18px' color={iconFill} />;
    } else if (icon === 'messages') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Users' mr='2' size='18px' color={iconFill} />;
    } else if (icon === 'tutorial') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Tutorial' mr='2' size='18px' color={iconFill} />;
    } 
    else {
      graphic = <Icon display='inline-block' icon='NullIcon' verticalAlign="middle" mr='2' size="16px" color={iconFill} />;
    }

    return graphic;
  }

  setHover(boolean) {
    this.setState({ hovered: boolean });
  }

  render() {
    const { icon, text, subtext, link, navigate, selected, invites, notificationsCount, contacts } = this.props;

    const color = contacts?.[text] ? `#${uxToHex(contacts[text].color)}` : "#000000";
    const graphic = this.getIcon(icon, selected, link, invites, notificationsCount, text, color);

    return (
      <Row
      py='2'
      px='2'
      cursor='pointer'
      onMouseEnter={() => this.setHover(true)}
      onMouseLeave={() => this.setHover(false)}
      backgroundColor={
        this.state.hovered || selected === link ? 'blue' : 'white'
      }
      onClick={navigate}
      width="100%"
      justifyContent="space-between"
      ref={this.result}
      >
      <Box display="flex" verticalAlign="middle" maxWidth="60%" flexShrink={0}>
        {graphic}
        <Text
        mono={(icon == 'profile' && text.startsWith('~'))}
        color={this.state.hovered || selected === link ? 'white' : 'black'}
        mr='1'
        >
          {text.startsWith("~") ? cite(text) : text}
        </Text>
        </Box>
        <Text pr='2'
        display="inline-block"
        verticalAlign="middle"
        color={this.state.hovered || selected === link ? 'white' : 'black'}
        width='100%'
        minWidth={0}
        textOverflow="ellipsis"
        whiteSpace="pre"
        overflow="hidden"
        maxWidth="40%"
        textAlign='right'
        >
          {subtext}
        </Text>
      </Row>
    );
  }
}

export default withState(OmniboxResult, [
  [useInviteState],
  [useHarkState, ['notificationsCount']],
  [useContactState]
]);