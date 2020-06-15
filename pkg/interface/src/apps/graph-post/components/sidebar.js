import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import _ from 'lodash';


export class Sidebar extends Component {
  constructor() {
    super();
  }

  onClickNew() {
    this.props.history.push('/~post/new');
  }

  renderKeys(keys) {
    const rooms = Array.from(keys).map((key) => {
      return (
        <div key={key}>
          <Link
            className={'no-underline '}
            to={'/~post/room/' + key}>
            {key}
          </Link>
        </div>
      );
    });
    return (
      <div>
        {rooms}
      </div>
    );
  }

  render() {
    const { props, state } = this;
    return (
      <div
        className={`h-100-minus-96-s h-100 w-100 overflow-x-hidden flex
      bg-gray0-d flex-column relative z1`}
      >
        <div className="w-100 bg-transparent pa4">
          <a
            className="dib f9 pointer green2 gray4-d mr4"
            onClick={this.onClickNew.bind(this)}
          >
            New Graph
          </a>
        </div>
        <div className="overflow-y-auto h-100">
          {this.renderKeys(props.keys)}
        </div>
      </div>
    );
  }
}
