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
    let blog = false;
    let finalUrl = this.props.location.pathname;

    if (last) {
      finalUrl = {
        pathName: finalUrl,
        state: last,
      }

      if ((last.lastMatch === '/~publish/:ship/:blog/:post') ||
          (last.lastMatch === '/~publish/:ship/:blog')){
        let blog = (last.lastParams.ship.slice(1) == window.ship)
          ?  _.get(this.props, `pubs[${last.lastParams.blog}]`, false)
          :  _.get(this.props, 
            `subs[${last.lastParams.ship.slice(1)}][${last.lastParams.blog}]`, false);
      }
    }

    if (this.props.location.pathname === '/~publish/new') {
      if (blog) {
        path.push({
          text: blog.info.title,
          url: `/~publish/${blog.info.owner}/${blog.info.filename}`,
        });
      }
      path.push(
        { text: 'New', url: finalUrl }
      );
    } else if (this.props.location.pathname === '/~publish/new/blog') {
      if (blog) {
        path.push({
          text: blog.info.title,
          url: `/~publish/${blog.info.owner}/${blog.info.filename}`,
        });
      }
      path.push(
        { text: 'New Blog', url: finalUrl }
      );
    } else if (this.props.location.pathname === '/~publish/new/post') {
      if (blog) {
        path.push({
          text: blog.info.title,
          url: `/~publish/${blog.info.owner}/${blog.info.filename}`,
        });
      }
      path.push(
        { text: 'New Post', url: finalUrl }
      );
    }
    return path;
  }

  render(){
    let pathData = (this.props.pathData)
      ?  this.props.pathData
      :  this.buildPathData();
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
