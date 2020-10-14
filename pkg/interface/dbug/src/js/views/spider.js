import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { msToDa, renderDuct } from '../lib/util';
import urbitOb from 'urbit-ob';
import { SearchableList } from '../components/searchable-list';

export class Spider extends Component {

  constructor(props) {
    super(props);
    this.state = {};

    this.loadThreads = this.loadThreads.bind(this);
    this.renderThreads = this.renderThreads.bind(this);
    this.killThread = this.killThread.bind(this);
  }

  componentDidMount() {
    const { threads } = this.props;
    if (Object.keys(threads).length === 0) {
      this.loadThreads();
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadThreads() {
    api.getThreads();
  }

  killThread(tid) {
    api.killThread(tid);
  }

  renderThreads(threads) {
    return Object.keys(threads).map(thread => {
      const kids = this.renderThreads(threads[thread]);
      return (<>
        <div>
          <button style={{margin: '4px'}} onClick={()=>{this.killThread(thread)}}>kill</button>
          {thread}
        </div>
        <div style={{paddingLeft: '16px'}}>{kids}</div>
      </>);
    });
  }

  render() {
    return (<>
      <button onClick={this.loadThreads}>refresh</button><br/>
      { Object.keys(this.props.threads).length === 0
        ? 'no running threads'
        : this.renderThreads(this.props.threads)
      }
    </>);
  }
}
