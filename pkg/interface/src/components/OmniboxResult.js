import React, { Component } from 'react';
import { Row, Icon, Text } from '@tlon/indigo-react';
import defaultApps from '../lib/default-apps';

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

  setHover(boolean) {
    this.setState({ hovered: boolean });
  }
  render() {
    const { icon, text, subtext, link, navigate, selected, dark } = this.props;

    let invertGraphic = {};

    if (icon.toLowerCase() !== 'dojo') {
      invertGraphic = (!dark && this.state.hovered) ||
      selected === link ||
      (dark && !(this.state.hovered || selected === link))
        ? { filter: 'invert(1)', paddingTop: 2 }
        : { filter: 'invert(0)', paddingTop: 2 };
    } else {
      invertGraphic =
        (!dark && this.state.hovered) ||
        selected === link ||
        (dark && !(this.state.hovered || selected === link))
          ? { filter: 'invert(0)', paddingTop: 2 }
          : { filter: 'invert(1)', paddingTop: 2 };
    }

    let graphic = <div />;
    if (defaultApps.includes(icon.toLowerCase()) || icon.toLowerCase() === 'links') {
      graphic = <img className="mr2 v-mid" height="12" width="12" src={`/~landscape/img/${icon.toLowerCase()}.png`} style={invertGraphic} />;
    } else {
      graphic = <Icon verticalAlign="middle" mr={2} size="12px" />;
    }
    return (
        <Row
          py='2'
          px='2'
          display='flex'
          flexDirection='row'
          style={{ cursor: 'pointer' }}
          onMouseEnter={() => this.setHover(true)}
          onMouseLeave={() => this.setHover(false)}
          backgroundColor={
            this.state.hovered || selected === link ? 'blue' : 'white'
          }
          onClick={navigate}
          width="100%"
          ref={this.result}
        >
          {this.state.hovered || selected === link ? (
            <>
            {graphic}
              <Text color='white' mr='1' style={{ 'flex-shrink': 0 }}>
                {text}
              </Text>
              <Text pr='2' color='white' width='100%' textAlign='right'>
                {subtext}
              </Text>
            </>
          ) : (
            <>
            {graphic}
              <Text mr='1' style={{ 'flex-shrink': 0 }}>{text}</Text>
              <Text pr='2' gray width='100%' textAlign='right'>
                {subtext}
              </Text>
            </>
          )}
        </Row>
    );
  }
}

export default OmniboxResult;
