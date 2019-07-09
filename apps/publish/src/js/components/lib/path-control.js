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
      };

      if ((last.lastMatch === '/~publish/:ship/:blog/:post') ||
          (last.lastMatch === '/~publish/:ship/:blog')){
        blog = (last.lastParams.ship.slice(1) == window.ship)
          ?  _.get(this.props, `pubs[${last.lastParams.blog}]`, false)
          :  _.get(this.props, 
            `subs[${last.lastParams.ship.slice(1)}][${last.lastParams.blog}]`, false);
      }
    }

    if (this.props.location.pathname === '/~publish/new-blog') {
      path.push(
        { text: 'New Blog', url: finalUrl }
      );
    } else if (this.props.location.pathname === '/~publish/new-post') {
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

  render() {
    let pathData = (this.props.pathData)
      ?  this.props.pathData
      :  this.buildPathData();
    let path = [];
    let key = 0;

    pathData.forEach((seg, i) => {
      let style = (i == 0)
        ?  {marginLeft: 16}
        :  {};
      if (i === pathData.length - 1)
        style.color = "black";

      path.push(
        <Link to={seg.url} key={key++}
          className="fl gray-30 label-regular" style={style}>
          {seg.text}
        </Link>
      );
      if (i < (pathData.length - 1)) {
        path.push(
          <img src="/~publish/arrow.png"
          className="fl ml1 mr1 relative"
          style={{top: 5}}
          key={key++}/>
        );
      }
    });

    let create = ((window.location.pathname === '/~publish/new-blog') ||
      (window.location.pathname === '/~publish/new-post')) ||
      (this.props.create === false)
      ?  false
      :  'post';

    return (
      <div className="fixed w-100 bg-white cf h-publish-header z-4"
        style={{top: 48}}>
        <PC create={create}/>
        <div className="path-control">
          {path}
        </div>
      </div>
    );
  }
}
