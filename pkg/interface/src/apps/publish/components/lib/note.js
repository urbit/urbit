import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { SidebarSwitcher } from '../../../../components/SidebarSwitch';
import { Spinner } from '../../../../components/Spinner';
import { Comments } from './comments';
import { NoteNavigation } from './note-navigation';
import moment from 'moment';
import ReactMarkdown from 'react-markdown';
import { cite } from '../../../../lib/util';

export class Note extends Component {
  constructor(props) {
    super(props);
    this.state = {
      deleting: false
    };
    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago';
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
        yy : '%d years'
      }
    });
    this.scrollElement = React.createRef();
    this.onScroll = this.onScroll.bind(this);
    this.deletePost = this.deletePost.bind(this);
  }

  componentDidMount() {
    this.componentDidUpdate();
    this.onScroll();
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if ((prevProps && prevProps.api !== props.api) || props.api) {
      if (!(props.notebooks[props.ship]?.[props.book]?.notes?.[props.note]?.file)) {
        props.api.publish.fetchNote(props.ship, props.book, props.note);
      }

      if (prevProps) {
        if ((prevProps.book !== props.book) ||
          (prevProps.note !== props.note) ||
          (prevProps.ship !== props.ship)) {
          const readAction = {
            read: {
              who: props.ship.slice(1),
              book: props.book,
              note: props.note
            }
          };
          props.api.publish.publishAction(readAction);
        }
      }
    }
  }

  onScroll() {
    const notebook = this.props.notebooks?.[this.props.ship]?.[this.props.book];
    const note = notebook?.notes?.[this.props.note];

    if (!note?.comments) {
      return;
    }

    const scrollTop = this.scrollElement.scrollTop;
    const clientHeight = this.scrollElement.clientHeight;
    const scrollHeight = this.scrollElement.scrollHeight;

    let atBottom = false;
    if (scrollHeight - scrollTop - clientHeight < 40) {
      atBottom = true;
    }

    const loadedComments = note.comments.length;
    const allComments = note['num-comments'];

    const fullyLoaded = (loadedComments === allComments);

    if (atBottom && !fullyLoaded) {
      this.props.api.publish.fetchCommentsPage(this.props.ship,
        this.props.book, this.props.note, loadedComments, 30);
    }
  }

  deletePost() {
    const { props } = this;
    const deleteAction = {
      'del-note': {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note
      }
    };
    const popout = (props.popout) ? 'popout/' : '';
    const baseUrl = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    this.setState({ deleting: true });
    this.props.api.publish.publishAction(deleteAction)
    .then(() => {
      props.history.push(baseUrl);
    });
  }

  render() {
    const { props } = this;
    const notebook = props.notebooks?.[props.ship]?.[props.book] || {};
    const comments = notebook?.notes?.[props.note]?.comments || false;
    const title = notebook?.notes?.[props.note]?.title || '';
    const author = notebook?.notes?.[props.note]?.author || '';
    const file = notebook?.notes?.[props.note]?.file || '';
    const date = moment(notebook.notes?.[props.note]?.['date-created']).fromNow() || 0;

    const contact = author.substr(1) in props.contacts
      ? props.contacts[author.substr(1)] : false;

    let name = author;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : author;
    }

    if (name === author) {
      name = cite(author);
    }

    if (!file) {
      return null;
    }

    const newfile = file.slice(file.indexOf(';>')+2);
    const prevId = notebook?.notes?.[props.note]?.['prev-note'] || null;
    const nextId = notebook?.notes?.[props.note]?.['next-note'] || null;
    const prevDate = moment(notebook?.notes?.[prevId]?.['date-created']).fromNow() || 0;
    const nextDate = moment(notebook?.notes?.[nextId]?.['date-created']).fromNow() || 0;

    const prev = (prevId === null)
      ?  null
      :  {
        id: prevId,
        title: notebook?.notes?.[prevId]?.title,
        date: prevDate
      };
      const next = (nextId === null)
        ?  null
        :  {
          id: nextId,
          title: notebook?.notes?.[nextId]?.title,
          date: nextDate
        };

    let editPost = null;
    const editUrl = props.location.pathname + '/edit';
    if (`~${window.ship}` === author) {
        editPost = <div className="dib">
        <Link className="green2 f9" to={editUrl}>Edit</Link>
        <p className="dib f9 red2 ml2 pointer"
          onClick={(() => this.deletePost())}
        >Delete</p>
        </div>;
    }

    const popout = (props.popout) ? 'popout/' : '';

    const hrefIndex = props.location.pathname.indexOf('/note/');
    const publishsubStr = props.location.pathname.substr(hrefIndex);
    const popoutHref = `/~publish/popout${publishsubStr}`;

    const hiddenOnPopout = props.popout ? '' : 'dib-m dib-l dib-xl';

    const baseUrl = `/~publish/${popout}notebook/${props.ship}/${props.book}`;
    return (
      <div
        className='h-100 overflow-y-scroll'
        onScroll={this.onScroll}
        ref={(el) => {
          this.scrollElement = el;
        }}
      >
          <SidebarSwitcher
            popout={props.popout}
            sidebarShown={props.sidebarShown}
            api={this.props.api}
            classes="absolute top-1 pl4"
          />
        <div className='h-100 flex flex-column items-center pa4'>
          <div className='w-100 flex justify-center pb6'>
            <Link className='f9 w-100 w-90-m w-90-l mw6 tl' to={baseUrl}>
              {'<- Notebook index'}
            </Link>
            <Link
              to={popoutHref}
              className={'dn absolute right-1 top-1 ' + hiddenOnPopout}
              target='_blank'
            >
              <img src='/~landscape/img/popout.png' height={16} width={16} />
            </Link>
          </div>
          <div className='w-100 mw6'>
            <div className='flex flex-column'>
              <div className='f9 mb1' style={{ overflowWrap: 'break-word' }}>
                {title}
              </div>
              <div className='flex mb6'>
                <div
                  className={
                    'di f9 gray2 mr2 ' + (contact.nickname ? null : 'mono')
                  }
                  title={author}
                  style={{ lineHeight: 1.6 }}
                >
                  {name}
                </div>
                <div className='di' style={{ lineHeight: 1 }}>
                  <span className='f9 gray2 dib'>{date}</span>
                  <span className='ml2 dib'>{editPost}</span>
                </div>
              </div>
            </div>
            <div className='md' style={{ overflowWrap: 'break-word' }}>
              <ReactMarkdown source={newfile} linkTarget={'_blank'} />
            </div>
            <NoteNavigation
              popout={props.popout}
              prev={prev}
              next={next}
              ship={props.ship}
              book={props.book}
            />
            <Comments
              enabled={notebook.comments}
              ship={props.ship}
              book={props.book}
              note={props.note}
              comments={comments}
              contacts={props.contacts}
              api={this.props.api}
            />
            <Spinner
              text='Deleting post...'
              awaiting={this.state.deleting}
              classes='absolute bottom-1 right-1 ba b--gray1-d pa2'
            />
          </div>
        </div>
      </div>
    );
  }
}

export default Note;
