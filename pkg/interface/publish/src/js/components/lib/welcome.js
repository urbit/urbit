import React, { Component } from 'react';


export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
  }
  render() {
    let wasWelcomed = localStorage.getItem("urbit-publish:wasWelcomed");
    if (wasWelcomed === null) {
      localStorage.setItem("urbit-publish:wasWelcomed", JSON.stringify(false));
      return wasWelcomed = false;
    } else {
      wasWelcomed = JSON.parse(wasWelcomed);
    }

    let notebooks = !!this.props.notebooks ? this.props.notebooks : {};

    return ((!wasWelcomed && this.state.show) && (notebooks.length !== 0)) ? (
      <div className="ma4 pa2 ba bg-gray5 b--gray4 bg-gray0-d b--gray2-d white-d">
        <p className="f9 lh-copy">Notebooks are best used for longer-form writing and text editing. Notes accept Markdown for formatting.</p>
        <p className="f9 pt2 dib fw6 pointer"
          onClick={(() => {
            localStorage.setItem("urbit-publish:wasWelcomed", JSON.stringify(true));
            this.setState({ show: false })
          })}>
          Close this message
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
