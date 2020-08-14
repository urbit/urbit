import React, { Component } from 'react';

export class Toggle extends Component {
  render() {
    const switchClasses = (this.props.boolean)
      ? 'relative checked bg-green2 br3 h1 toggle v-mid z-0'
      : 'relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0';

    return (
      <input
        type="checkbox"
        style={{ WebkitAppearance: 'none', width: 28 }}
        className={switchClasses}
        onChange={this.props.change}
      />
    );
  }
}

export default Toggle;
