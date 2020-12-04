import React, { Component } from 'react';
import { Box, Row, Icon, Text } from '@tlon/indigo-react';
import defaultApps from '~/logic/lib/default-apps';
import Sigil from '~/logic/lib/sigil';

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

  getIcon(icon, selected, link, invites, notifications) {
    const iconFill = (this.state.hovered || (selected === link)) ? 'white' : 'black';
    const sigilFill = (this.state.hovered || (selected === link)) ? '#3a8ff7' : '#ffffff';
    const bulletFill = (this.state.hovered || (selected === link)) ? 'white' : 'blue';

    const inviteCount = [].concat(...Object.values(invites).map(obj => Object.values(obj)));

    let graphic = <div />;
    if (defaultApps.includes(icon.toLowerCase())
        || icon.toLowerCase() === 'links'
        || icon.toLowerCase() === 'terminal')
    {
      icon = (icon === 'Link')     ? 'Links' :
             (icon === 'Terminal') ? 'Dojo'  : icon;
      graphic = <Icon display="inline-block" verticalAlign="middle" icon={icon} mr='2' size='16px' color={iconFill} />;
    } else if (icon === 'inbox') {
      graphic = <Box display='flex' verticalAlign='middle'>
        <Icon display='inline-block' verticalAlign='middle' icon='Inbox' mr='2' size='16px' color={iconFill} />
        {(notifications > 0 || inviteCount.length > 0) && (
          <Icon display='inline-block' icon='Bullet' style={{ position: 'absolute', top: -5, left: 5 }} color={bulletFill} />
        )}
      </Box>;
    } else if (icon === 'logout') {
      graphic = <Icon display="inline-block" verticalAlign="middle" icon='SignOut' mr='2' size='16px' color={iconFill} />;
    } else if (icon === 'profile') {
      graphic = <Sigil color={sigilFill} classes='dib flex-shrink-0 v-mid mr2' ship={window.ship} size={16} icon padded />;
    } else if (icon === 'home') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Mail' mr='2' size='16px' color={iconFill} />;
    } else if (icon === 'notifications') {
      graphic = <Icon display='inline-block' verticalAlign='middle' icon='Inbox' mr='2' size='16px' color={iconFill} />;
    } else {
      graphic = <Icon display='inline-block' icon='NullIcon' verticalAlign="middle" mr='2' size="16px" color={iconFill} />;
    }

    return graphic;
  }

  setHover(boolean) {
    this.setState({ hovered: boolean });
  }

  render() {
    const { icon, text, subtext, link, navigate, selected, invites, notifications } = this.props;

    const graphic = this.getIcon(icon, selected, link, invites, notifications);

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
      ref={this.result}
      >
        {graphic}
        <Text
        display="inline-block"
        verticalAlign="middle"
        color={this.state.hovered || selected === link ? 'white' : 'black'}
        maxWidth="60%"
        style={{ flexShrink: 0 }}
        mr='1'
        >
          {text}
        </Text>
        <Text pr='2'
        display="inline-block"
        verticalAlign="middle"
        color={this.state.hovered || selected === link ? 'white' : 'black'}
        width='100%'
        textAlign='right'
        >
          {subtext}
        </Text>
      </Row>
    );
  }
}

export default OmniboxResult;
