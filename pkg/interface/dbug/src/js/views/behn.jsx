import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { msToDa, renderDuct } from '../lib/util';
import urbitOb from 'urbit-ob';
import { SearchableList } from '../components/searchable-list';

export class Behn extends Component {

  constructor(props) {
    super(props);
    this.state = {};

    this.loadTimers = this.loadTimers.bind(this);
  }

  componentDidMount() {
    const { timers } = this.props;
    if (timers.length === 0) {
      this.loadTimers();
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadTimers() {
    api.getTimers();
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;

    const items = props.timers.map(timer => {
      const duct = renderDuct(timer.duct);
      return {key: duct, jsx: (<div class="flex">
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {msToDa(timer.date)}
        </div>
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {duct}
        </div>
      </div>)};
    });

    return (
      <table><tbody>
        <SearchableList placeholder="duct" items={items}>
          <button onClick={this.loadTimers}>refresh</button>
        </SearchableList>
      </tbody></table>
    );
  }
}
