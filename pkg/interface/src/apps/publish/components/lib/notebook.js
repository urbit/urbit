import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { SidebarSwitcher } from '../../../../components/SidebarSwitch';
import { NotebookPosts } from './notebook-posts';
import { Subscribers } from './subscribers';
import { Settings } from './settings';
import { cite } from '../../../../lib/util';

export class Notebook extends Component {
  constructor(props) {
    super(props);

    this.onScroll = this.onScroll.bind(this);
    this.unsubscribe = this.unsubscribe.bind(this);
  }

  onScroll() {
    const notebook = this.props.notebooks[this.props.ship][this.props.book];
    const scrollTop = this.scrollElement.scrollTop;
    const clientHeight = this.scrollElement.clientHeight;
    const scrollHeight = this.scrollElement.scrollHeight;

    let atBottom = false;
    if (scrollHeight - scrollTop - clientHeight < 40) {
      atBottom = true;
    }
    if (!notebook.notes && this.props.api) {
      this.props.api.publish.fetchNotebook(this.props.ship, this.props.book);
      return;
    }

    const loadedNotes = Object.keys(notebook?.notes).length || 0;
    const allNotes = notebook?.['notes-by-date'].length || 0;

    const fullyLoaded = (loadedNotes === allNotes);

    if (atBottom && !fullyLoaded) {
      this.props.api.publish.fetchNotesPage(this.props.ship, this.props.book, loadedNotes, 30);
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if ((prevProps && (prevProps.api !== props.api)) || props.api) {
      const notebook = props.notebooks?.[props.ship]?.[props.book];
      if (!notebook?.subscribers) {
        props.api.publish.fetchNotebook(props.ship, props.book);
      }
    }
  }

  componentDidMount() {
    this.componentDidUpdate();
    const notebook = this.props.notebooks?.[this.props.ship]?.[this.props.book];
    if (notebook?.notes) {
      this.onScroll();
    }
  }

  unsubscribe() {
    const action = {
      unsubscribe: {
        who: this.props.ship.slice(1),
        book: this.props.book
      }
    };
    this.props.api.publish.publishAction(action);
    this.props.history.push('/~publish');
  }

  render() {
    const { props } = this;

    // popout logic
    const hrefIndex = props.location.pathname.indexOf('/notebook/');
    const publishsubStr = props.location.pathname.substr(hrefIndex);
    const popoutHref = `/~publish/popout${publishsubStr}`;

    const hiddenOnPopout = props.popout ? '' : 'dib-m dib-l dib-xl';

    const notebook = props.notebooks?.[props.ship]?.[props.book];

    const tabStyles = {
      posts: 'bb b--gray4 b--gray2-d gray2 pv4 ph2',
      about: 'bb b--gray4 b--gray2-d gray2 pv4 ph2',
      subscribers: 'bb b--gray4 b--gray2-d gray2 pv4 ph2',
      settings: 'bb b--gray4 b--gray2-d pr2 gray2 pv4 ph2'
    };
    tabStyles[props.view] = 'bb b--black b--white-d black white-d pv4 ph2';

    let inner = null;
    switch (props.view) {
      case 'posts': {
        const notesList = notebook?.['notes-by-date'] || [];
        const notes = notebook?.notes || null;
        inner = <NotebookPosts notes={notes}
                  popout={props.popout}
                  list={notesList}
                  host={props.ship}
                  notebookName={props.book}
                  contacts={props.notebookContacts}
                />;
        break;
      }
      case 'about':
        inner = <p className="f8 lh-solid">{notebook?.about}</p>;
        break;
      case 'subscribers':
        inner = <Subscribers
                  host={this.props.ship}
                  book={this.props.book}
                  notebook={notebook}
                  permissions={this.props.permissions}
                  groups={this.props.groups}
                  api={this.props.api}
                />;
        break;
      case 'settings':
        inner = <Settings
                  host={this.props.ship}
                  book={this.props.book}
                  notebook={notebook}
                  groups={this.props.groups}
                  contacts={this.props.contacts}
                  associations={this.props.associations}
                  history={this.props.history}
                  api={this.props.api}
                />;
        break;
      default:
        break;
    }

    // displaying nicknames, sigil colors for contacts
    const contact = props.ship.substr(1) in props.notebookContacts
      ? props.notebookContacts[props.ship.substr(1)] : false;
    let name = props.ship;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : props.ship;
    }

    if (name === props.ship) {
      name = cite(props.ship);
    }

    const popout = (props.popout) ? 'popout/' : '';
    const base = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    const about = base + '/about';
    const subs = base + '/subscribers';
    const settings = base + '/settings';
    const newUrl = base + '/new';

    let newPost = null;
    if (notebook?.['writers-group-path'] in props.groups) {
      const writers = notebook?.['writers-group-path'];
      if (props.groups?.[writers].has(window.ship)) {
        newPost = (
          <Link
            to={newUrl}
            className='NotebookButton bg-light-green green2 ph2 pt3'
          >
            New Post
          </Link>
        );
      }
    }

    const unsub = (window.ship === props.ship.slice(1))
      ?  null
      :  <button onClick={this.unsubscribe}
             className="NotebookButton bg-white bg-gray0-d black white-d ba b--black b--gray2-d ml3"
         >
           Unsubscribe
         </button>;

    const subsComponent = (this.props.ship.slice(1) !== window.ship)
      ? null
      : <Link to={subs} className={tabStyles.subscribers}>
          Subscribers
        </Link>;

    const settingsComponent = (this.props.ship.slice(1) !== window.ship)
      ? null
      : <Link to={settings} className={tabStyles.settings}>
          Settings
        </Link>;

    return (
      <div
        className='overflow-y-scroll h-100'
        style={{ paddingLeft: 16, paddingRight: 16 }}
        onScroll={this.onScroll}
        ref={(el) => {
          this.scrollElement = el;
        }}
      >
        <div className='w-100 dn-m dn-l dn-xl inter pt4 pb6 f9'>
          <Link to='/~publish'>{'<- All Notebooks'}</Link>
        </div>
        <div style={{ paddingTop: 11 }}>
          <SidebarSwitcher
            popout={props.popout}
            sidebarShown={props.sidebarShown}
            api={this.props.api}
            classes="absolute top-1"
          />
        </div>
        <div
          className='center mw6 f9 h-100'
          style={{ paddingLeft: 16, paddingRight: 16 }}
        >
          <Link
            className={'dn absolute right-1 top-1 ' + hiddenOnPopout}
            to={popoutHref}
            target='_blank'
          >
            <img src='/~landscape/img/popout.png' height={16} width={16} />
          </Link>
          <div className='h-100 pt0 pt8-m pt8-l pt8-xl no-scrollbar'>
            <div className='flex justify-between' style={{ marginBottom: 32 }}>
              <div className='flex-col'>
                <div className='mb1'>{notebook?.title}</div>
                <span>
                  <span className='gray3 mr1'>by</span>
                  <span
                    className={contact.nickname ? null : 'mono'}
                    title={props.ship}
                  >
                    {name}
                  </span>
                </span>
              </div>
              <div className='flex'>
                {newPost}
                {unsub}
              </div>
            </div>

            <div className='flex' style={{ marginBottom: 24 }}>
              <Link to={base} className={tabStyles.posts}>
                All Posts
              </Link>
              <Link to={about} className={tabStyles.about}>
                About
              </Link>
              {subsComponent}
              {settingsComponent}
              <div
                className='bb b--gray4 b--gray2-d gray2 pv4 ph2'
                style={{ flexGrow: 1 }}
              ></div>
            </div>

            <div
              style={{ height: 'calc(100% - 188px)' }}
              className='f9 lh-solid'
            >
              {inner}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Notebook;
