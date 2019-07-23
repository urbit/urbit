import React, { Component } from 'react';
import classnames from 'classnames';

class Subscribe extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (this.props.actionType === 'subscribe') {
      return (
        <p className="label-small b pointer"
          onClick={this.props.subscribe}>
          Subscribe
        </p>
      );
    } else if (this.props.actionType === 'unsubscribe') {
      return (
        <p className="label-small b pointer"
          onClick={this.props.unsubscribe}>
          Unsubscribe
        </p>
      );
    } else {
      return null;
    }
  }
}

class Subscribers extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let subscribers = (this.props.subNum === 1)
      ?  `${this.props.subNum} Subscriber`
      :  `${this.props.subNum} Subscribers`;

    if (this.props.action !== null) {
      return (
        <p className="label-small b pointer" onClick={this.props.action}>
          {subscribers}
        </p>
      );
    } else {
      return (
        <p className="label-small b">{subscribers}</p>
      );
    }
  }
}

class Settings extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (this.props.action !== null) {
      return (
        <p className="label-small b pointer" onClick={this.props.action}>
          Settings
        </p>
      );
    } else {
      return null;
    }
  }
}

export class BlogData extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div className="flex-col">
        <p className="label-small">By ~{this.props.host}</p>
        <Subscribers action={this.props.viewSubs} subNum={this.props.subNum}/>
        <Settings action={this.props.viewSettings}/>
        <Subscribe actionType={this.props.subscribeAction}
          subscribe={this.props.subscribe}
          unsubscribe={this.props.unsubscribe}
        />
      </div>
    );
  }
}
