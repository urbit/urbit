import React, { Component } from 'react';
import { Row, Icon, Text } from '@tlon/indigo-react';
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

  getIcon(icon, dark, selected, link) {
    const iconFill = (this.state.hovered || (selected === link)) ? 'white' : 'black';
    const sigilFill = (this.state.hovered || (selected === link)) ? '#3a8ff7' : '#ffffff';

    let graphic = <div />;
    if (defaultApps.includes(icon.toLowerCase()) || icon.toLowerCase() === 'links') {
      icon = (icon === 'Dojo') ? 'ChevronEast' : icon;
      icon = (icon === 'Link') ? 'Links' : icon;

      graphic = <Icon display="inline-block" verticalAlign="middle" icon={icon} mr='2' size='16px' color={iconFill} />;
    } else if (icon === 'logout') {
      graphic = <Icon display="inline-block" verticalAlign="middle" icon='ArrowWest' mr='2' size='16px' color={iconFill} />;
    } else if (icon === 'profile') {
      graphic = <Sigil color={sigilFill} classes='dib v-mid mr2' ship={window.ship} size={16} />;
    } else {
      graphic = <Icon verticalAlign="middle" mr='2' size="16px" color={iconFill} />;
    }

    return graphic;
  }

  setHover(boolean) {
    this.setState({ hovered: boolean });
  }

  render() {
    const { icon, text, subtext, link, navigate, selected, dark } = this.props;

    const graphic = this.getIcon(icon, dark, selected, link);

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
