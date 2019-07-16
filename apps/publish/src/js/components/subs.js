import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { withRouter } from 'react-router';
import { HeaderMenu } from '/components/lib/header-menu';
import moment from 'moment';

const HM = withRouter(HeaderMenu);

export class Subs extends Component {
  constructor(props) {
    super(props);

    this.accept = this.accept.bind(this);
    this.reject = this.reject.bind(this);
    this.unsubscribe = this.unsubscribe.bind(this);

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
    let invites = this.props.invites.map((inv) => {
      return {
        type: 'invite',
        url: `/~publish/~${inv.who}/${inv.coll}`,
        host: `~${inv.who}`,
        title: inv.title,
        blogId: inv.coll,
      };
    })

    let data = Object.keys(this.props.subs).map((ship) => {
      let perShip = Object.keys(this.props.subs[ship]).map((blogId) => {
        let blog = this.props.subs[ship][blogId];
        return {
          type: 'regular',
          url: `/~publish/${blog.info.owner}/${blogId}`,
          title: blog.info.title,
          host: blog.info.owner,
          lastUpdated: moment(blog["last-update"]).fromNow(),
          blogId: blogId,
        }
      });
      return perShip;
    });
    let merged = data.flat();
    return invites.concat(merged);
  }

  accept(host, blogId) {
    let subscribe = {
      subscribe: {
        who: host.slice(1),
        coll: blogId,
      }
    };
    this.props.api.action("publish", "publish-action", subscribe);
  }

  reject(host, blogId) {
    let reject = {
      "reject-invite": {
        who: host.slice(1),
        coll: blogId,
      }
    };
    this.props.api.action("publish", "publish-action", reject);
  }

  unsubscribe(host, blogId) {
    let unsubscribe = {
      unsubscribe: {
        who: host.slice(1),
        coll: blogId,
      }
    };
    this.props.api.action("publish", "publish-action", unsubscribe);
  }

  render() {
    let blogData = this.buildBlogData();

    let blogs = this.buildBlogData().map( (data, i) => {
      let bg = (i % 2 == 0)
        ?  "bg-v-light-gray"
        :  "bg-white";
      let cls = "w-100 flex " + bg;
      if (data.type === 'regular') {
        return (
          <div className={cls} key={i}>
            <div className="fl mw-336" style={{flexBasis: 336}}>
              <Link to={data.url}>
                <p className="body-regular-400 one-line pr3">
                   {data.title}
                </p>
              </Link>
            </div>
            <p className="fl body-regular-400" style={{flexBasis:336}}>
              {data.host}
            </p>
            <p className="fl body-regular-400" style={{flexBasis:336}}>
              {data.lastUpdated}
            </p>
            <p className="fl body-regular-400 pointer" 
              style={{flexBasis:336}}
              onClick={this.unsubscribe.bind(this, data.host, data.blogId)}>
              Unsubscribe
            </p>
          </div>
        );
      } else if (data.type === 'invite') {
        return (
          <div className={cls} key={i}>
            <div className="fl body-regular-400" style={{flexBasis: 336}}>
              <Link to={data.url}>
                <div className="mw-336 one-line pr3">
                  <span className="body-large green-medium"> • </span>
                  <span className="body-regular-400">Invite to</span>
                  <span className="body-regular">
                    {data.title}
                  </span>
                </div>
              </Link>
            </div>
            <p className="fl body-regular-400" style={{flexBasis:336}}>
              {data.host}
            </p>
            <p className="fl body-regular-400" style={{flexBasis:336}}>
            </p>
            <p className="fl body-regular-400" style={{flexBasis:336}}>
              <span className="green underline pointer" 
                onClick={this.accept.bind(this, data.host, data.blogId)}>
                Accept
              </span>
              <span>   </span>
              <span className="red underline pointer" 
                onClick={this.reject.bind(this, data.host, data.blogId)}>
                Reject
              </span>
            </p>
          </div>
        );
      }
    });


    return (
      <div>
        <HM/>
        <div className="absolute w-100" style={{top:124}}>
          <div className="flex-col">
            <div className="w-100">
              <h2 className="gray-50"
                style={{marginLeft: 16, marginTop: 32, marginBottom: 16}}>
                Subscriptions
              </h2>
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
              <p className="fl gray-50 body-regular-400" style={{flexBasis:336}}>
              </p>
            </div>

            {blogs}
          </div>
        </div>
      </div>
    );
  }
}
