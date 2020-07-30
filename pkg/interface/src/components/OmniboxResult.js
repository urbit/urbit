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
  }

  setHover(boolean) {
    this.setState({ hovered: boolean });
  }
  render() {
    const { icon, text, subtext, link, navigate, selected, dark } = this.props;

    const invertGraphic = ((!dark && this.state.hovered || selected === link) || (dark && !(this.state.hovered || selected === link)))
      ? { filter: 'invert(1)' }
      : { filter: 'invert(0)' };

    let graphic = <div />;
    if (defaultApps.includes(icon.toLowerCase())) {
      graphic = <img className="invert-d mr2 v-mid" height="12" width="12" src={`/~landscape/img/${icon.toLowerCase()}.png`} style={invertGraphic} />;
    } else {
      graphic = <Icon />;
    }
    return (
        <Row
          py='2'
          px='2'
          display='block'
          style={{ cursor: "pointer" }}
          onMouseEnter={() => this.setHover(true)}
          onMouseLeave={() => this.setHover(false)}
          backgroundColor={
            this.state.hovered || selected === link ? "blue" : "white"
          }
          onClick={navigate}>
          {this.state.hovered || selected === link ? (
            <>
            {graphic}
              <Text color='white' mr='1'>
                {text}
              </Text>
              <Text style={{ float: "right" }} color='white'>
                {subtext}
              </Text>
            </>
          ) : (
            <>
            {graphic}
              <Text mr='1'>{text}</Text>
              <Text style={{ float: "right" }} gray>
                {subtext}
              </Text>
            </>
          )}
        </Row>
    );
  }
}

export default OmniboxResult;
