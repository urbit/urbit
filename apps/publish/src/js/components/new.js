import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';

const PC = withRouter(PathControl);
 
export class New extends Component {
  constructor(props){
    super(props);
  }

  render() {
    let last = _.get(this.props, 'location.state', false);
    let blogLink;
    let postLink;

    if (last) {
      blogLink = {
        pathname: "/~publish/new/blog",
        state: last,
      };
      postLink = {
        pathname: "/~publish/new/post",
        state: last,
      };
    } else {
      blogLink = "/~publish/new/blog";
      postLink = "/~publish/new/post";
    }

    return (
      <div>
        <div className="cf w-100 bg-white h-publish-header">
          <PC pathData={false} {...this.props}/>
        </div>
        <div className="h-inner dt center mw-688 w-100">
          <div className="flex-col dtc v-mid">
            <Link to={blogLink}>
              <h2 className="v-mid">-> New Blog</h2>
            </Link>
            <hr className="gray-30"/>
            <Link to={postLink}>
              <h2 className="v-mid">-> New Post</h2>
            </Link>
          </div>
        </div>
      </div>
    );
  }
}
