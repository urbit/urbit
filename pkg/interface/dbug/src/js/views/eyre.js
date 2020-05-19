import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { msToDa, renderDuct } from '../lib/util';
import urbitOb from 'urbit-ob';
import { SearchableList } from '../components/searchable-list';

export class Eyre extends Component {

  constructor(props) {
    super(props);
    this.state = {};

    this.loadBindings = this.loadBindings.bind(this);
  }

  componentDidMount() {
    const { bindings } = this.props;
    if (bindings.length === 0) {
      this.loadBindings();
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadBindings() {
    api.getBindings();
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;

    const items = props.bindings.map(binding => {

      return {key: binding.location + ' ' + binding.action, jsx: (<div class="flex">
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {binding.location}
        </div>
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {binding.action}
        </div>
      </div>)};
    });

    return (
      <table><tbody>
        <SearchableList placeholder="binding" items={items}>
          <button onClick={this.loadBindings}>refresh</button>
        </SearchableList>
      </tbody></table>
    );
  }
}
