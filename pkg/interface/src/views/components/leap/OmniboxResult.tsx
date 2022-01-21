import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { Contacts, Invites } from '@urbit/api';
import React, { Component, ReactElement } from 'react';
import defaultApps from '~/logic/lib/default-apps';
import Sigil from '~/logic/lib/sigil';
import { cite, uxToHex } from '~/logic/lib/util';
import { IconRef } from '~/types/util';
import withState from '~/logic/lib/withState';
import useContactState from '~/logic/state/contact';
import useInviteState from '~/logic/state/invite';

function OmniboxResultChord(props: {
  label: string;
  modifier?: string;

}) {
  const { label, modifier } = props;

  return (
    <Text display={['none', 'inline']} color="white" ml="2">
      {label}
      { modifier ? (<Text display="inline-block" ml="1" p="1" borderRadius="1" color="blue" backgroundColor="white">{modifier}</Text>) : null}
      <Text display="inline-block" ml="1" color="blue" borderRadius="1" p="1" backgroundColor="white">↩</Text>
    </Text>
  );
}

interface OmniboxResultProps {
  contacts: Contacts;
  cursor: string;
  icon: string;
  invites: Invites;
  link: string;
  navigate: () => void;
  selected: string;
  setSelection: () =>  void;
  subtext: string;
  text: string;
  shiftLink?: string;
  shiftDescription?: string;
  description?: string;
  hasNotifications?: boolean;
  hasUnreadDms?: boolean;
}

interface OmniboxResultState {
  isSelected: boolean;
  hovered: boolean;
}

export class OmniboxResult extends Component<OmniboxResultProps, OmniboxResultState> {
  result: React.RefObject<typeof Row>;
  constructor(props: OmniboxResultProps) {
    super(props);
    this.state = {
      isSelected: false,
      hovered: false
    };
    this.setHover = this.setHover.bind(this);
    this.result = React.createRef();
  }

  componentDidUpdate(prevProps: OmniboxResultProps): void {
    const { props, state } = this;
    if (
      prevProps &&
      !state.hovered &&
      prevProps.selected !== props.selected &&
      props.selected === props.link
      && this.result.current
    ) {
      // @ts-ignore ref is forwarded as never, investigate later
      this.result.current.scrollIntoView({ block: 'nearest' });
    }
  }

  getIcon(
    icon: string,
    selected: string,
    link: string,
    text: string,
    color: string
  ): (any) {
    const iconFill =
      (this.state.hovered || selected === link) ? 'white' : 'black';

    let graphic: ReactElement = <div />;
    if (
      defaultApps.includes(icon.toLowerCase()) ||
      icon.toLowerCase() === 'links' ||
      icon.toLowerCase() === 'terminal' ||
      icon === 'btc-wallet'
    ) {
      if (icon === 'Link') {
        icon = 'Collection';
      } else if (icon === 'Terminal') {
        icon = 'Dojo';
      } else if (icon === 'btc-wallet') {
        icon = 'Bitcoin';
      }
      graphic = (
        <Icon
          display='inline-block'
          verticalAlign='middle'
          icon={icon as IconRef}
          mr={2}
          size='18px'
          color={iconFill}
        />
      );
    } else if (icon === 'logout') {
      graphic = (
        <Icon
          display='inline-block'
          verticalAlign='middle'
          icon='LogOut'
          mr={2}
          size='18px'
          color={iconFill}
        />
      );
    } else if (icon === 'profile') {
      text = text.startsWith('Profile') ? window.ship : text;
      graphic = (
        <Sigil
          color={color}
          classes='dib flex-shrink-0 v-mid mr2'
          ship={text}
          size={18}
          icon
          padding={2}
        />
      );
    } else if (icon === 'home') {
      graphic = (
        <Icon
          display='inline-block'
          verticalAlign='middle'
          icon='Home'
          mr={2}
          size='18px'
          color={iconFill}
        />
      );
    } else if (icon === 'notifications') {
      graphic = (
        <Box mr="2" height="18px" width="18px" position="relative" display="inline-block">
          <Icon
            display='inline-block'
            verticalAlign='middle'
            icon='Notifications'
            size='18px'
            color={iconFill}
          />
          {this.props.hasNotifications ? (
            <Box position="absolute" right="-6px" top="-4px">
              <Icon icon="Bullet" color={(this.state.hovered || selected === link) ? 'white' : 'blue'} />
            </Box>
          ) : null}
        </Box>
      );
    } else if (icon === 'messages') {
      graphic = (
        <Box mr="2" height="18px" width="18px" position="relative" display="inline-block">
          <Icon
            display='inline-block'
            verticalAlign='middle'
            icon='Messages'
            mr={2}
            size='18px'
            color={iconFill}
          />
          {this.props.hasUnreadDms ? (
            <Box position="absolute" right="-6px" top="-4px">
              <Icon icon="Bullet" color={(this.state.hovered || selected === link) ? 'white' : 'blue'} />
            </Box>
          ) : null}
        </Box>
      );
    } else {
      graphic = (
        <Icon
          display='inline-block'
          icon='NullIcon'
          verticalAlign='middle'
          mr={2}
          size='16px'
          color={iconFill}
        />
      );
    }

    return graphic;
  }

  setHover(boolean: boolean): void {
    this.setState({ hovered: boolean });
  }

  render(): ReactElement {
    const {
      icon,
      text,
      subtext,
      link,
      cursor,
      navigate,
      selected,
      contacts,
      setSelection,
      shiftDescription,
      description = 'Go',
      shiftLink
    } = this.props;

    const color = contacts?.[text]
      ? `#${uxToHex(contacts[text].color)}`
      : '#000000';
    const graphic = this.getIcon(
      icon,
      selected,
      link,
      text,
      color
    );

    return (
      <Row
        p={1}
        cursor={cursor}
        onMouseMove={() => setSelection()}
        onMouseLeave={() => this.setHover(false)}
        backgroundColor={
          this.state.hovered || selected === link ? 'blue' : 'white'
        }
        onClick={navigate}
        width='100%'
        height="32px"
        alignItems="center"
        justifyContent='space-between'
        // @ts-ignore indigo-react doesn't allow us to pass refs
        ref={this.result}
      >
        <Box
          display='flex'
          verticalAlign='middle'
          maxWidth='60%'
          flexShrink={0}
        >
          {graphic}
          <Text
            mono={icon == 'profile' && text.startsWith('~')}
            color={this.state.hovered || selected === link ? 'white' : 'black'}
            display='inline-block'
            verticalAlign='middle'
            width='100%'
            overflow='hidden'
            textOverflow='ellipsis'
            whiteSpace='pre'
            mr={1}
          >
            {text.startsWith('~') ? cite(text) : text}
          </Text>
        </Box>

        <Text
          pr={1}
          display='inline-block'
          verticalAlign='middle'
          color={this.state.hovered || selected === link ? 'white' : 'black'}
          width='100%'
          minWidth={0}
          textOverflow='ellipsis'
          whiteSpace='pre'
          overflow='hidden'
          maxWidth='60%'
          textAlign='right'
        >
          {(selected === link) ? (
            <>
              <OmniboxResultChord label={description} />
              {(shiftLink && shiftDescription) ? (<OmniboxResultChord label={shiftDescription} modifier="Shift" />) : null}
            </>
          ) : subtext}
        </Text>
      </Row>
    );
  }
}

export default withState(OmniboxResult, [
  [useInviteState],
  [useContactState]
]);
