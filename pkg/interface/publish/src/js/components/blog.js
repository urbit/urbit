import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/lib/post-preview';
import _ from 'lodash';
import { PathControl } from '/components/lib/path-control';
import { BlogData } from '/components/lib/blog-data';
import { BlogNotes } from '/components/lib/blog-notes';
import { BlogSubs } from '/components/lib/blog-subs';
import { BlogSettings } from '/components/lib/blog-settings';
import { withRouter } from 'react-router';
import { NotFound } from '/components/not-found';
import { Link } from 'react-router-dom';

const PC = withRouter(PathControl);
const NF = withRouter(NotFound);
const BN = withRouter(BlogNotes);
const BS = withRouter(BlogSettings)


export class Blog extends Component {
  constructor(props){
    super(props);

    this.state = {
      view: 'notes',
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
    this.viewSubs = this.viewSubs.bind(this);
    this.viewSettings = this.viewSettings.bind(this);
    this.viewNotes = this.viewNotes.bind(this);

    this.blog = null;
  }

  handleEvent(diff) {
    if (diff.data.total) {
      let blog = diff.data.total.data;
      this.blog = blog;
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
      if (diff.data.remove.post) {
       // XX TODO 
      } else {
        this.props.history.push("/~publish/recent");
      }
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
      ?  _.get(this.props, `pubs["${blogId}"]`, false)
      :  _.get(this.props, `subs["${ship}"]["${blogId}"]`, false);


    if (!(blog) && (ship === window.ship)) {
      this.setState({notFound: true});
      return;
    } else if (this.blog && !blog) {
      this.props.history.push("/~publish/recent");
      return;
    }

    this.blog = blog;

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
      ?  _.get(this.props, `pubs["${blogId}"]`, false)
      :  _.get(this.props, `subs["${ship}"]["${blogId}"]`, false);

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
    } else {
      this.blog = blog;
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
      ?  _.get(this.props, `pubs["${this.props.blogId}"]`, false)
      :  _.get(this.props, `subs["${this.props.ship}"]["${this.props.blogId}"]`, false);

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

  viewSubs() {
    this.setState({view: 'subs'});
  }

  viewSettings() {
    this.setState({view: 'settings'});
  }

  viewNotes() {
    this.setState({view: 'notes'});
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

      let contributors = `~${this.props.ship}`;
      let create = (this.props.ship === window.ship);

      let subNum = _.get(data.blog, 'subscribers.length', 0);

      let foreign = _.get(this.props,
        `subs["${this.props.ship}"]["${this.props.blogId}"]`, false);

      let actionType = false;
      if (this.state.temporary) {
        actionType = 'subscribe';
      } else if ((this.props.ship !== window.ship) && foreign) {
        actionType = 'unsubscribe';
      }

      let viewSubs = (this.props.ship === window.ship)
        ? this.viewSubs
        : null;

      let viewSettings = (this.props.ship === window.ship)
        ? this.viewSettings
        : null;
      
      if (this.state.view === 'notes') {
        return (
          <div>
            <PC pathData={data.pathData} create={create}/>
            <div className="absolute w-100"
              style={{top:124, paddingLeft: 16, paddingRight: 16, paddingTop: 32}}>
              <div className="flex-col">
                <h2 style={{wordBreak: "break-word"}}>
                  {data.blogTitle}
                </h2>
                <div className="flex" style={{marginTop: 22}}>
                  <BlogData
                    host={this.props.ship}
                    viewSubs={viewSubs}
                    subNum={subNum}
                    viewSettings={viewSettings}
                    subscribeAction={actionType}
                    subscribe={this.subscribe}
                    unsubscribe={this.unsubscribe}
                  />
                </div>
                <BN ship={this.props.ship} posts={data.postProps} />
              </div>
            </div>
          </div>
        );
      } else if (this.state.view === 'subs') {
        let subscribers = _.get(data, 'blog.subscribers', []);
        return (
          <div>
            <PC pathData={data.pathData} create={create}/>
            <div className="absolute w-100"
              style={{top:124, paddingLeft: 16, paddingRight: 16, paddingTop: 32}}>
              <div className="flex-col">
                <h2 style={{wordBreak: "break-word"}}>
                  {data.blogTitle}
                </h2>
                <div className="flex" style={{marginTop: 22}}>
                  <BlogData
                    host={this.props.ship}
                    viewSubs={viewSubs}
                    subNum={subNum}
                    viewSettings={viewSettings}
                    subscribeAction={actionType}
                    subscribe={this.subscribe}
                    unsubscribe={this.unsubscribe}
                  />
                </div>
                <BlogSubs back={this.viewNotes}
                  subs={subscribers}
                  blogId={this.props.blogId}
                  title={data.blogTitle}
                  api={this.props.api}/>
              </div>
            </div>
          </div>
        );
      } else if (this.state.view === 'settings') {
        return (
          <div>
            <PC pathData={data.pathData} create={create}/>
            <div className="absolute w-100"
              style={{top:124, paddingLeft: 16, paddingRight: 16, paddingTop: 32}}>
              <div className="flex-col">
                <h2 style={{wordBreak: "break-word"}}>
                  {data.blogTitle}
                </h2>
                <div className="flex" style={{marginTop: 22}}>
                  <BlogData
                    host={this.props.ship}
                    viewSubs={viewSubs}
                    subNum={subNum}
                    viewSettings={viewSettings}
                    subscribeAction={actionType}
                    subscribe={this.subscribe}
                    unsubscribe={this.unsubscribe}
                  />
                </div>
                <BS back={this.viewNotes}
                  blogId={this.props.blogId}
                  title={data.blogTitle}
                  api={this.props.api}/>
              </div>
            </div>
          </div>
        );
      }
    }
  }
}

