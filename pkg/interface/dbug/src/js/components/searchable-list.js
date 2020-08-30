import React, { Component } from 'react';

export class SearchableList extends Component {
  // expected props:
  // items: [{key: 'some key', jsx: <w/e>}, ...]
  // placeholder: ''

  constructor(props) {
    super(props);
    this.state = {
      query: ''
    };

    this.updateQuery = this.updateQuery.bind(this);
  }

  updateQuery(event) {
    this.setState({ query: event.target.value });
  }

  render() {
    const { state, props } = this;

    const searchBar = (
      <input
        type="text"
        placeholder={props.placeholder}
        onChange={this.updateQuery}
        value={state.query}
        style={{border: '1px solid black'}}
      />
    );

    let items = props.items.filter(item => {
      return state.query.split(' ').reduce((match, query) => {
        return match && ('' + item.key).includes(query);
      }, true);
    })
    items = items.map(item =>
      (<div key={item.key} style={{marginTop: '4px'}}>{item.jsx}</div>)
    );

    return (<div style={{position: 'relative', border: '1px solid grey', padding: '4px'}}>
      {props.children}
      <div>{searchBar} ({items.length})</div>
      <div>{items.length === 0 ? 'none' : items}</div>
    </div>);
  }
}
