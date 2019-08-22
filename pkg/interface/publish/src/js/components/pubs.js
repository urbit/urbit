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
          <div className="fl body-regular-400 mw-336 w-336 pr3">
            <Link to={data.url}>
              <p className="ml3 mw-336">
                <span>{data.title}</span>
              </p>
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

    let invites = (this.props.invites.length > 0);
    let unread = (this.props.unread.length > 0);
    
    return (
      <div>
        <HM invites={invites} unread={unread}/>
        <div className="absolute w-100" style={{top:124}}>
          <div className="flex-column">
            <div className="w-100">
              <h2 className="gray-50"
                style={{marginLeft: 16, marginTop:32, marginBottom: 16}}>
                Notebooks
              </h2>
            </div>
            <div className="w-100 flex">
              <p className="fl gray-50 body-regular-400" style={{flexBasis:336}}>
                <span className="ml3">Title</span>
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
      </div>
    );
  }
}
