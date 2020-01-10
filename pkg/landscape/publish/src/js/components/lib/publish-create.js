import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { withRouter } from 'react-router';

export class PublishCreate extends Component {
  constructor(props){
    super(props);
  }

  render () {
    if (!this.props.create) {
      return (
        <div className="w-100">
          <p className="publish">Publish</p>
        </div>
      );
    } else if (this.props.create == 'blog') {
      let link = {
        pathname: "/~publish/new-blog",
        state: {
          lastPath: this.props.location.pathname,
          lastMatch: this.props.match.path,
          lastParams: this.props.match.params,
        },
      };
      return (
        <div className="w-100">
          <p className="publish">Publish</p>
          <Link to={link}>
            <p className="create">+New Notebook</p>
          </Link>
        </div>
      );
    } else if (this.props.create == 'post') {
      let link = {
        pathname: "/~publish/new-post",
        state: {
          lastPath: this.props.location.pathname,
          lastMatch: this.props.match.path,
          lastParams: this.props.match.params,
        },
      };
      return (
        <div className="w-100">
          <p className="publish">Publish</p>
          <Link to={link}>
            <p className="create">+New Note</p>
          </Link>
        </div>
      );
    }
  }
}
