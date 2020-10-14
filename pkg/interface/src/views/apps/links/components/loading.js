import React, { Component } from 'react';
import { MessageScreen } from './lib/message-screen';

export class LoadingScreen extends Component {
  render() {
    return (<MessageScreen text="Loading..." />);
  }
}
