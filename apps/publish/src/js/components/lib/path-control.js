import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { withRouter } from 'react-router';
import { PublishCreate } from '/components/lib/publish-create';
import _ from 'lodash';

const PC = withRouter(PublishCreate);

export class PathControl extends Component {
  constructor(props){
    super(props);

  }

  retrievePost(post, coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].posts[post].post.info;
    } else {
      return this.props.subs[who][coll].posts[post].post.info;
    }
  }

  retrieveColl(coll, who) {
    if (who === window.ship) {
      return this.props.pubs[coll].info;
    } else {
      return this.props.subs[who][coll].info;
    }
  }

  buildPathData(){

    let path = [
      { text: "Home", url: "/~publish/recent" },
    ];

    let last = _.get(this.props, 'location.state', false);
    let blogTitle = false;
    let blogUrl   = false;
    let finalUrl = this.props.location.pathname;

    if (last) {
      finalUrl = {
        pathName: finalUrl,
        state: last,
      }

      if ((last.lastMatch === '/~publish/:ship/:blog/:post') ||
          (last.lastMatch === '/~publish/:ship/:blog')){
        let blog = this.retrieveColl(last.lastParams.blog, last.lastParams.ship.slice(1));
        blogTitle = blog.title, 
        blogUrl = `/~publish/${blog.owner}/${blog.filename}` 
      }
    }

    if (this.props.location.pathname === '/~publish/new') {
      if (blogTitle && blogUrl) {
        path.push({text: blogTitle, url: blogUrl});
      }
      path.push(
        { text: 'New', url: finalUrl }
      );
    } else if (this.props.location.pathname === '/~publish/new/blog') {
      if (blogTitle && blogUrl) {
        path.push({text: blogTitle, url: blogUrl});
      }
      path.push(
        { text: 'New Blog', url: finalUrl }
      );
    } else if (this.props.location.pathname === '/~publish/new/post') {
      if (blogTitle && blogUrl) {
        path.push({text: blogTitle, url: blogUrl});
      }
      path.push(
        { text: 'New Post', url: finalUrl }
      );
    } else if (this.props.match.path === '/~publish/:ship/:blog') {
      let blogId = this.props.match.params.blog;
      let postId = this.props.match.params.post;
      let ship = this.props.match.params.ship.slice(1);
      let blog = this.retrieveColl(blogId, ship);
      path.push(
        {text: blog.title, url: `/~publish/${blog.owner}/${blog.filename}` }
      );

    } else if (this.props.match.path === '/~publish/:ship/:blog/:post') {
      let blogId = this.props.match.params.blog;
      let postId = this.props.match.params.post;
      let ship = this.props.match.params.ship.slice(1);
      let blog = this.retrieveColl(blogId, ship);
      let post = this.retrievePost(postId, blogId, ship);
      path.push(
        {text: blog.title, url: `/~publish/${blog.owner}/${blog.filename}` }
      );
      path.push(
        {text: post.title, url: `/~publish/${blog.owner}/${blog.filename}/${post.filename}` }
      );

    }

    return path;
  }

  render(){
    let pathData = this.buildPathData();

    let path = [];
    let key = 0;

    pathData.forEach((seg, i) => {
      let style = (i == 0)
        ?  {marginLeft: 16}
        :  null;
      style = (i == (pathData.length - 1))
        ?  {color: "black"}
        :  style;
      path.push(
        <Link to={seg.url} key={key++} className="fl gray-30 label-regular" style={style}>
          {seg.text}
        </Link>
      );
      if (i < (pathData.length - 1)){
        path.push(
          <p className="fl gray-30 label-regular" key={key++}>
            ->
          </p>
        );
      }
    });

    return (
      <div>
        <PC/>
        <div className="path-control">
          {path}
        </div>
      </div>
    );
  }
}
