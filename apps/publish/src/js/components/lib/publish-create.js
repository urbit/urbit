import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { withRouter } from 'react-router';
import { store } from '/store';

export class PublishCreate extends Component {
  constructor(props){
    super(props);
  }

  render () {
    let link = {
      pathname: "/~publish/new",
      state: {
        lastPath: this.props.location.pathname,
        lastMatch: this.props.match.path,
        lastParams: this.props.match.params,
      },
    };

    return (
      <div className="w-100">
        <p className="publish">Publish</p>
        <Link to={link} >
          <p className="create">+Create</p>
        </Link>
      </div>
    );
  }
}
