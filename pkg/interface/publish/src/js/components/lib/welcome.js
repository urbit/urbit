import React, { Component } from 'react';


export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
    this.disableWelcome = this.disableWelcome.bind(this);
  }

  disableWelcome() {
    this.setState({ show: false });
    localStorage.setItem("urbit-publish:wasWelcomed", JSON.stringify(true));
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
      <div className="ma4 pa2 white-d bg-welcome-green bg-gray1-d">
        <p className="f8 lh-copy">Notebooks are for longer-form writing and discussion. Each Notebook is a collection of Markdown-formatted notes with optional comments.</p>
        <p className="f8 pt2 dib pointer bb"
          onClick={(() => this.disableWelcome())}>
          Close this
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
