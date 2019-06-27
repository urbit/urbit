import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom'
import { withRouter } from 'react-router';
import { HeaderMenu } from '/components/lib/header-menu';
import moment from 'moment';

const HM = withRouter(HeaderMenu);

export class Pubs extends Component {
  constructor(props) {
    super(props);

    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago'
        },
        s : 'just now',
        future : 'in %s',
        m  : '1m',
        mm : '%dm',
        h  : '1h',
        hh : '%dh',
        d  : '1d',
        dd : '%dd',
        M  : '1 month',
        MM : '%d months',
        y  : '1 year',
        yy : '%d years',
      }
    });
  }

  buildBlogData() {
    let data = Object.keys(this.props.pubs).map((blogId) => {
      let blog = this.props.pubs[blogId];
      return {
        url: `/~publish/${blog.info.owner}/${blogId}`,
        title: blog.info.title,
        host: blog.info.owner,
        lastUpdated: moment(blog["last-update"]).fromNow(),
      }
    });
    return data;
  }


  render() {
    let blogData = this.buildBlogData();

    let blogs = this.buildBlogData().map( (data, i) => {
      let bg = (i % 2 == 0)
        ?  "bg-v-light-gray"
        :  "bg-white";
      let cls = "w-100 flex " + bg;
      return (
        <div className={cls} key={i}>
          <div className="fl body-regular-400" style={{flexBasis: 336}}>
            <Link to={data.url}>
               {data.title}
            </Link>
          </div>
          <p className="fl body-regular-400" style={{flexBasis:336}}>
            {data.host}
          </p>
          <p className="fl body-regular-400" style={{flexBasis:336}}>
            {data.lastUpdated}
          </p>
        </div>
      );
    });


    return (
      <div>
        <div className="cf w-100 bg-white h-publish-header">
          <HM/>
        </div>
        <div className="flex-col">
          <div className="w-100 h-80">
            <h2 className="gray-50">My Blogs</h2>
          </div>
          <div className="w-100 flex">
            <p className="fl gray-50 body-regular-400" style={{flexBasis:336}}>
               Title
            </p>
            <p className="fl gray-50 body-regular-400" style={{flexBasis:336}}>
              Host
            </p>
            <p className="fl gray-50 body-regular-400" style={{flexBasis:336}}>
              Last Updated
            </p>
          </div>
          {blogs}
        </div>
      </div>
    );
  }
}
