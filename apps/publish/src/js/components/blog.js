import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/lib/post-preview';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { withRouter } from 'react-router';
import { NotFound } from '/components/not-found';
import { Link } from 'react-router-dom';

const PC = withRouter(PathControl);
const NF = withRouter(NotFound);

class Subscribe extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (this.props.actionType === 'subscribe') {
      return (
        <p className="label-small-2 b pointer"
          onClick={this.props.subscribe}>
          Subscribe
        </p>
      );
    } else if (this.props.actionType === 'unsubscribe') {
      return (
        <p className="label-small-2 b pointer"
          onClick={this.props.unsubscribe}>
          Unsubscribe
        </p>
      );
    } else {
      return null;
    }
  }
}

export class Blog extends Component {
  constructor(props){
    super(props);

    this.state = {
      awaiting: false,
      postProps: [],
      blogTitle: '',
      blogHost: '',
      pathData: [],
      temporary: false,
      awaitingSubscribe: false,
      awaitingUnsubscribe: false,
      notFound: false,
    };

    this.subscribe = this.subscribe.bind(this);
    this.unsubscribe = this.unsubscribe.bind(this);
  }

  handleEvent(diff) {
    if (diff.data.total) {
      let blog = diff.data.total.data;
      this.setState({
        postProps: this.buildPosts(blog),
        blog: blog,
        blogTitle: blog.info.title,
        blogHost: blog.info.owner,
        awaiting: false,
        pathData: [
          { text: "Home", url: "/~publish/recent" },
          { text: blog.info.title, 
            url: `/~publish/${blog.info.owner}/${blog.info.filename}` }
        ],
      });

      this.props.setSpinner(false);
    } else if (diff.data.remove) {
      // XX TODO

    }
  }

  handleError(err) {
    this.props.setSpinner(false);
    this.setState({notFound: true});
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.notFound) return;

    let ship = this.props.ship;
    let blogId = this.props.blogId;

    let blog = (ship === window.ship)
      ?  _.get(this.props, `pubs[${blogId}]`, false)
      :  _.get(this.props, `subs[${ship}][${blogId}]`, false);

    if (!(blog) && (ship === window.ship)) {
      this.setState({notFound: true});
      return;
    };

    if (this.state.awaitingSubscribe && blog) {
      this.setState({
        temporary: false,
        awaitingSubscribe: false,
      });

      this.props.setSpinner(false);
    }
  }

  componentWillMount() {
    let ship = this.props.ship;
    let blogId = this.props.blogId;
    let blog = (ship == window.ship)
      ?  _.get(this.props, `pubs[${blogId}]`, false)
      :  _.get(this.props, `subs[${ship}][${blogId}]`, false);

    if (!(blog) && (ship === window.ship)) {
      this.setState({notFound: true});
      return;
    };


    let temporary = (!(blog) && (ship != window.ship));

    if (temporary) {
      this.setState({
        awaiting: {
          ship: ship,
          blogId: blogId,
        },
        temporary: true,
      });

      this.props.setSpinner(true);

      this.props.api.bind(`/collection/${blogId}`, "PUT", ship, "publish",
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    }
  }

  buildPosts(blog){
    if (!blog) {
      return [];
    }
    let pinProps = blog.order.pin.map((postId) => {
      let post = blog.posts[postId];
      return this.buildPostPreviewProps(post, blog, true);
    });

    let unpinProps = blog.order.unpin.map((postId) => {
      let post = blog.posts[postId];
      return this.buildPostPreviewProps(post, blog, false);
    });

    return pinProps.concat(unpinProps);
  }

  buildPostPreviewProps(post, blog, pinned){
    return {
      postTitle: post.post.info.title,
      postName:  post.post.info.filename,
      postBody: post.post.body,
      numComments: post.comments.length,
      collectionTitle: blog.info.title,
      collectionName:  blog.info.filename,
      author: post.post.info.creator,
      blogOwner: blog.info.owner,
      date: post.post.info["date-created"],
      pinned: pinned,
    }
  }

  buildData(){
    let blog = (this.props.ship == window.ship)
      ?  _.get(this.props, `pubs[${this.props.blogId}]`, false)
      :  _.get(this.props, `subs[${this.props.ship}][${this.props.blogId}]`, false);

    if (this.state.temporary) {
      return {
        blog: this.state.blog,
        postProps: this.state.postProps,
        blogTitle: this.state.blogTitle,
        blogHost: this.state.blogHost,
        pathData: this.state.pathData,
      };
    } else {
      if (!blog) {
        return false;
      }
      return {
        blog: blog,
        postProps: this.buildPosts(blog),
        blogTitle: blog.info.title,
        blogHost: blog.info.owner,
        pathData: [
          { text: "Home", url: "/~publish/recent" },
          { text: blog.info.title, 
            url: `/~publish/${blog.info.owner}/${blog.info.filename}` }
        ],
      };
    }
  }

  subscribe() {
    let sub = {
      subscribe: {
        who: this.props.ship,
        coll: this.props.blogId,
      }
    }
    this.props.setSpinner(true);
    this.setState({awaitingSubscribe: true}, () => {
      this.props.api.action("publish", "publish-action", sub);
    });
  }

  unsubscribe() {
    let unsub = {
      unsubscribe: {
        who: this.props.ship,
        coll: this.props.blogId,
      }
    }
    this.props.api.action("publish", "publish-action", unsub);
    this.props.history.push("/~publish/recent");
  }

  render() {

    if (this.state.notFound) {
      return (
        <NF/>
      );
    } else if (this.state.awaiting) {
      return null;
    } else {
      let data = this.buildData();

      let posts = data.postProps.map((post, key) => {
        return (
          <PostPreview
            post={post}
            key={key}
          />
        );
      });

      if ((posts.length === 0) && (this.props.ship === window.ship)) {
        let link = {
          pathname: "/~publish/new-post",
          state: {
            lastPath: this.props.location.pathname,
            lastMatch: this.props.match.path,
            lastParams: this.props.match.params,
          }
        }
        posts.push(
          <div key={0} className="w-336 relative">
            <hr className="gray-10" style={{marginBottom:18}}/>
            <Link to={link}>
              <p className="body-large b">
                -> Create First Post
              </p>
            </Link>
          </div>
        );
      }

      let contributors = `~${this.props.ship}`;
      let create = (this.props.ship === window.ship);

      let subscribers = 'None';
      let subNum = _.get(data.blog, 'subscribers.length', 0);

      if (subNum === 1) {
        subscribers = `~${data.blog.subscribers[0]}`;
      } else if (subNum === 2) {
        subscribers = `~${data.blog.subscribers[0]} and 1 other`;
      } else if (subNum > 2) {
        subscribers = `~${data.blog.subscribers[0]} and ${subNum-1} others`;
      }

      let foreign = _.get(this.props,
        `subs[${this.props.ship}][${this.props.blogId}]`, false);

      let actionType = false;
      if (this.state.temporary) {
        actionType = 'subscribe';
      } else if ((this.props.ship !== window.ship) && foreign) {
        actionType = 'unsubscribe';
      }

      return (
        <div>
          <PC pathData={data.pathData} create={create}/>
          <div className="absolute w-100"
            style={{top:124, marginLeft: 16, marginRight: 16, marginTop: 32}}>
            <div className="flex-col">
              <h2 style={{wordBreak: "break-word", marginRight:32}}>
                {data.blogTitle}
              </h2>
              <div className="flex" style={{marginTop: 22}}>
                <div className="flex-col" style={{flexBasis: 160, marginRight:16}}>
                  <p className="gray-50 label-small-2 b">Host</p>
                  <p className="label-small-2">{data.blogHost}</p>
                </div>
                <div style={{flexBasis: 160, marginRight:16}}>
                  <p className="gray-50 label-small-2 b">Contributors</p>
                  <p className="label-small-2">{contributors}</p>
                </div>
                <div style={{flexBasis: 160, marginRight: 16}}>
                  <p className="gray-50 label-small-2 b">Subscribers</p>
                  <p className="label-small-2">{subscribers}</p>
                  <Subscribe actionType={actionType}
                    subscribe={this.subscribe}
                    unsubscribe={this.unsubscribe}
                  />
                </div>
              </div>
              <div className="flex flex-wrap" style={{marginTop: 48}}>
                {posts}
              </div>
            </div>
          </div>
        </div>
      );
    }
  }
}

