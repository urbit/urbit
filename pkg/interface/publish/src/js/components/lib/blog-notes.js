import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/lib/post-preview';
import { Link } from 'react-router-dom';

export class BlogNotes extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (!this.props.posts ||
        ((this.props.posts.length === 0) &&
         (this.props.ship === window.ship))) {
      let link = {
        pathname: "/~publish/new-post",
        state: {
          lastPath: this.props.location.pathname,
          lastMatch: this.props.match.path,
          lastParams: this.props.match.params,
        }
      }

      return (
        <div className="flex flex-wrap">
          <div className="w-336 relative">
            <hr className="gray-10" style={{marginTop: 48, marginBottom:25}}/>
            <Link to={link}>
              <p className="body-large b">
                -> Create First Post
              </p>
            </Link>
          </div>
        </div>
      );
    }

    let posts = this.props.posts.map((post, key) => {
      return (
        <PostPreview post={post} key={key}/>
      );
    });

    return (
      <div className="flex flex-wrap" style={{marginTop: 48}}>
        {posts}
      </div>
    );
  }
}
