import React, { Component } from 'react';
import { BrowserRouter, Route } from 'react-router-dom';
import classnames from 'classnames';
import { Link } from 'react-router-dom';

export class Header extends Component {
  constructor(props) {
    super(props);

    console.log("header.props", props);
  }

  retrievePost(post, coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].posts[post].post;
    } else {
      return this.props.subs[who][coll].posts[post].post;
    }
  }

  retrieveComments(post, coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].posts[post].comments;
    } else {
      return this.props.subs[who][coll].posts[post].comments;
    }
  }

  retrieveColl(coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].info;
    } else {
      return this.props.subs[who][coll].info;
    }
  }

  render() {
    return (
      <div className="cf w-100">
        <div className="fl w-100">
          <p className="fl body-large b gray-50 publish">Publish</p>
          <p className="label-regular b fr">+ Create</p>
        </div>

        <Route exact path="/~publish/recent"
          render={ (props) => {
            return (
              <div className="fl w-100 flex">
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/recent">
                    <p className="fl w-100 h2 label-regular">
                      Recent
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/subs">
                    <p className="fl w-100 h2 label-regular gray-30">
                      Subscriptions
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/pubs">
                    <p className="fl w-100 h2 label-regular gray-30">
                      My Blogs
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ flexGrow: 1}}>
                </div>
              </div>
            );
          }} />

        <Route exact path="/~publish/subs"
          render={ (props) => {
            return (
              <div className="fl w-100 flex">
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/recent">
                    <p className="fl w-100 h2 label-regular gray-30">
                      Recent
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/subs">
                    <p className="fl w-100 h2 label-regular">
                      Subscriptions
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/pubs">
                    <p className="fl w-100 h2 label-regular gray-30">
                      My Blogs
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ flexGrow: 1}}>
                </div>
              </div>
            );
          }} />

        <Route exact path="/~publish/pubs"
          render={ (props) => {
            return (
              <div className="fl w-100 flex">
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/recent">
                    <p className="fl w-100 h2 label-regular gray-30">
                      Recent
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb b-gray-30" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/subs">
                    <p className="fl w-100 h2 label-regular gray-30">
                      Subscriptions
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ width: 16 }}>
                </div>
                <div className="fl bb" style={{ flexBasis: 148 }}>
                  <Link to="/~publish/pubs">
                    <p className="fl w-100 h2 label-regular">
                      My Blogs
                    </p>
                  </Link>
                </div>
                <div className="fl bb b-gray-30" style={{ flexGrow: 1}}>
                </div>
              </div>
            );
          }} />

        <Route exact path="/~publish/:ship/:blog"
          render={ (props) => {
            let ship = props.match.params.ship.slice(1);
            let blogId = props.match.params.blog;

            let blogLink = `/~publish/~${ship}/${blogId}`;
            let blog = this.retrieveColl(blogId, ship);
            let blogName = blog.title;

            return (
              <div className="fl w-100 flex b-gray-30 bb">
                <Link to="/~publish/recent">
                  <p className="fl gray-30 label-regular" style={{ marginLeft: 16}}>
                    Home
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <Link to={blogLink}>
                  <p className="fl label-regular">
                    {blogName}
                  </p>
                </Link>
              </div>
            );
          }} />

        <Route exact path="/~publish/:ship/:blog/:post"
          render={ (props) => {
            let ship = props.match.params.ship.slice(1);
            let blogId = props.match.params.blog;
            let postId = props.match.params.post;

            let blogLink = `/~publish/~${ship}/${blogId}`;
            let blog = this.retrieveColl(blogId, ship);
            let blogName = blog.title;

            let postLink = `/~publish/~${ship}/${blogId}/${postId}`;
            let post = this.retrievePost(postId, blogId, ship);
            let postName = post.info.title;

            return (
              <div className="fl w-100 flex b-gray-30 bb">
                <Link to="/~publish/recent">
                  <p className="fl gray-30 label-regular" style={{ marginLeft: 16}}>
                    Home
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <Link to={blogLink}>
                  <p className="fl gray-30 label-regular">
                    {blogName}
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <Link to={postLink}>
                  <p className="fl label-regular">
                    {postName}
                  </p>
                </Link>
              </div>
            );
          }} />
      
        <Route exact path="/~publish/new"
          render={ (props) => {
            return (
              <div className="fl w-100 flex b-gray-30 bb">
                <Link to="/~publish/recent">
                  <p className="fl gray-30 label-regular" style={{ marginLeft: 16}}>
                    Home
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <p className="fl label-regular">
                  New
                </p>
              </div>
            );
          }} />

        <Route exact path="/~publish/:ship/:blog/new"
          render={ (props) => {
            let ship = props.match.params.ship.slice(1);
            let blogId = props.match.params.blog;

            let blogLink = `/~publish/~${ship}/${blogId}`;
            let blog = this.retrieveColl(blogId, ship);
            let blogName = blog.title;

            return (
              <div className="fl w-100 flex b-gray-30 bb">
                <Link to="/~publish/recent">
                  <p className="fl gray-30 label-regular" style={{ marginLeft: 16}}>
                    Home
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <Link to={blogLink}>
                  <p className="fl gray-30 label-regular">
                    {blogName}
                  </p>
                </Link>
                <p className="fl gray-30 label-regular">
                  ->
                </p>
                <p className="fl label-regular">
                  New Post
                </p>
              </div>
            );
          }} />
      </div>
    );
  }
}
