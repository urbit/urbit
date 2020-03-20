import React, { Component } from 'react'

export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
  }

  render() {
    let firstTime = window.startupMessage;
    return (firstTime && this.state.show)
    ? (
      <div className={"fl ma2 bg-white bg-gray0-d white-d overflow-hidden " +
      "ba b--black b--gray1-d pa2 w-100 mw6 lh-copy"}>
      <p className="f9">You've just come into possession of a virtual computer you own completely. You can run it on a laptop, you can run it on a server, but it doesn't really matter.</p>
      <p className="f9 pt2">With the key you used to access this space, you can never lose this computer. You can never lose what's on this computer.</p>
      <p className="f9 pt2">To begin, you should probably pop into a chat and verify there are signs of life in this new place. If you were invited by a friend, you probably already have access to a few.</p>
      <p className="f9 pt2">If you don't, feel free to <a className="no-underline bb b--black b--gray1-d" href="/~chat/join/~/~dopzod/urbit-help">join our lobby.</a></p>
      <p className="dib f9 pt2 bb b--black b--gray1-d pointer"
      onClick={(() => {
        window.api.action("launch", "json", "disable welcome message");
        this.setState({show: false});
      })}>Close this note</p>
      </div>
    )
    : ( <div/>)
  }
}

export default Welcome
