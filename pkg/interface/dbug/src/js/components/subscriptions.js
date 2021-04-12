import React, { Component } from 'react';
import { SearchableList } from '../components/searchable-list';
import { renderDuct } from '../lib/util';

export class Subscriptions extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate(prevProps) {
    //
  }

  render() {
    const props = this.props;

    const incoming = props.in.map(inc => {
      return {key: '~'+inc.ship + ' ' + inc.path, jsx: (
        <div class="flex">
          <div class="flex-auto" style={{maxWidth: '10%'}}>
            ~{inc.ship}
          </div>
          <div class="flex-auto" style={{maxWidth: '30%'}}>
            {inc.path}
          </div>
          <div class="flex-auto" style={{maxWidth: '60%'}}>
            {renderDuct(inc.duct)}
          </div>
        </div>
      )};
    });

    const outgoing = props.out.map(out => {
      return {key: `~${out.ship} ${out.app} ${out.wire} ${out.path}`, jsx: (
        <div class="flex">
          <div class="flex-auto" style={{maxWidth: '35%'}}>
            {out.wire}
          </div>
          <div class="flex-auto" style={{maxWidth: '10%'}}>
            ~{out.ship}
          </div>
          <div class="flex-auto" style={{maxWidth: '10%'}}>
            {out.app}
          </div>
          <div class="flex-auto" style={{maxWidth: '35%'}}>
            {out.path}
          </div>
          <div class="flex-auto" style={{maxWidth: '10%'}}>
            {out.acked ? 'acked' : 'not acked'}
          </div>
        </div>
      )};
    });

    return (<div>
      <h4>Incoming</h4>
      <SearchableList placeholder="ship / path" items={incoming} />
      <h4>Outgoing</h4>
      <SearchableList placeholder="ship / app / wire / path" items={outgoing} />
    </div>);
  }
}

export default Links;