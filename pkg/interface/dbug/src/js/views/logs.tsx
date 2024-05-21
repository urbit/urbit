import { Component } from 'react';
import { showDirectoryPicker } from 'file-system-access';
import { msToDa } from '../lib/util';
import { store } from '../store';

export class Logs extends Component {
  view: { oldest: number; newest: number; clicked: string; };

  constructor(props) {
    super(props);
    this.state = {};

    this.loadLogs = this.loadLogs.bind(this);
    this.clearLogs = this.clearLogs.bind(this);
    this.clickEvent = this.clickEvent.bind(this);

    this.view = {
      oldest: null,
      newest: null,
      clicked: null,
    }
  }

  componentDidMount() {
    //
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
    console.log('updated component');
  }

  async loadLogs() {
    const dir = await showDirectoryPicker();
    if (dir.kind !== 'directory' || dir.name !== 'verb-logger') {
      return alert('Not a real verb-logger put directory!');
    }

    for await (const [dude, logs] of dir.entries()) {
      if (logs.kind !== 'directory') continue;
      if (dude === '.DS_Store') continue;

      let files = [];
      for await (const [key, log] of logs.entries()) {
        if (log.kind !== 'file') continue;
        if (key.slice(-5) !== '.json') continue;
        files.push({ key, log });
      }
      files = files.sort((a, b) => (a.key < b.key) ? -1 : 1);

      for (const { key, log } of files) {
        const start = parseInt(key.slice(0, -5));
        const contents = JSON.parse(await (await log.getFile()).text());
        const gill = contents.ship + '/' + contents.dude;
        store.addLogs(gill, contents.from, contents.events);
      }
    }
  }

  clearLogs() {
    store.clearLogs();
  }

  clickEvent(eventId) {
    if (this.view.clicked === eventId)
      this.view.clicked = null;
    else
      this.view.clicked = eventId;
    this.setState({ view: this.view });
  }

  renderViewControls() {
    const oldest = this.view.oldest || store.state.logsRange.oldest;
    const newest = this.view.newest || store.state.logsRange.newest;

    return (<div id="controls">
      from {msToDa(oldest, true)}
      <br/>
      <input
        type="range"
        onChange={(e) => {
          this.view.oldest = Number.parseInt(e.target.value);
          this.setState({ view: this.view });
        }} step="1"
        min={store.state.logsRange.oldest}
        max={store.state.logsRange.newest}
        value={oldest}
      />
      <br/>
      <input
        type="range"
        onChange={(e) => {
          const newest = Number.parseInt(e.target.value);
          if (newest === store.state.logsRange.newest)
            this.view.newest = null;  //  stick to latest
          else
            this.view.newest = newest;
          this.setState({ view: this.view });
        }} step="1"
        min={store.state.logsRange.oldest}
        max={store.state.logsRange.newest}
        value={newest}
      />
      <br/>
      thru {msToDa(newest, true) + (this.view.newest ? '' : ' (live)')}
    </div>);
  }

  renderEvent(gill, event, n) {
    const oldest = this.view.oldest || store.state.logsRange.oldest;
    const newest = this.view.newest || store.state.logsRange.newest;
    const span = newest - oldest;

    const id = `${gill}#${event.act}`;
    const clicked = this.view.clicked === id;

    let deets =
      `
      act: ${event.act}<br/>
      now: ${msToDa(event.now, true)}<br/>
      src: ${event.src}${event.sap}<br/>
      `;
    switch (event.kind) {
      case 'on-poke':
        deets = deets +
          `
          mark: %${event.deets.mark}<br/>
          mug: 0x${event.deets.mug}
          `;
        break;
      //
      case 'on-watch':
      case 'on-leave':
        deets = deets + `path: ${event.deets}`;
        break;
      //
      case 'on-agent':
        deets = deets +
          `
          wire: ${event.deets.wire}<br/>
          <u>${event.deets.sign}</u><br/>
          `;
        switch (event.deets.sign) {
          case 'poke-ack':
          case 'watch-ack':
            deets = deets + (event.deets.deets ? 'ack' : 'nack');
            break;
          //
          case 'fact':
            deets = deets +
              `
              mark: %${event.deets.deets.mark}<br/>
              mug: 0x${event.deets.deets.mug}
              `;
            break;
        }
        break;
    }

    deets = deets + `<br/><u>${event.effects.length} effects</u>`;
    if (event.effects.length > 0) {
      deets = deets + ':';
      for (const effect of event.effects) {
        deets = deets +
          `
          <details><summary>
            ${effect.kind}, ${effect.deets.wire || effect.deets.mark || ''}
          </summary>
          `;
        switch (effect.kind) {
          case 'poke':
            deets = deets +
              `
              wire: ${effect.deets.wire}<br/>
              gill: ${effect.deets.gill}<br/>
              mark: %${effect.deets.mark}<br/>
              mug: 0x${effect.deets.mug}
              `;
            break;
          //
          case 'watch':
            deets = deets +
              `
              wire: ${effect.deets.wire}<br/>
              gill: ${effect.deets.gill}<br/>
              path: ${effect.deets.path}
              `;
            break;
          //
          case 'leave':
            deets = deets +
              `
              wire: ${effect.deets.wire}<br/>
              gill: ${effect.deets.gill}
              `;
            break;
          //
          case 'fact':
            deets = deets +
              `
              paths: ${effect.deets.paths.join(', ')}<br/>
              mark: %${effect.deets.mark}<br/>
              mug: 0x${effect.deets.mug}
              `;
            break;
          //
          case 'kick':
            deets = deets + `paths: ${effect.deets.paths.join(', ')}`;
            break;
          //
          case 'arvo':
            deets = deets +
              `
              wire: ${effect.deets.wire}<br/>
              task: %${effect.deets.vane} %${effect.deets.task}
              `;
            break;
        }
        deets = deets + '</details>';
      }
    }

    // if (event.kind === 'on-agent') {
    //   switch (event.deets.sign) {
    //     case 'fact':
    //       vent.style.borderColor = '#' + event.deets.deets.mug.slice(0, 6);
    //       break;
    //     //
    //     case 'poke-ack':
    //     case 'watch-ack':
    //       vent.style.borderColor = event.deets.deets ? 'green' : 'red';
    //       break;
    //     //
    //     case 'kick':
    //       vent.style.borderColor = 'purple';
    //       break;
    //   }
    // }

    const left = (((event.now - oldest) / span) * 98);
    return (<div
      className={'event' + (clicked ? ' focus' : '') + (left > 50 ? ' right' : '')}
      onClick={() => this.clickEvent(id)}
      style={{
          left: left+'%',
          top: ((n % 18) * 5)+'%',
        }}
    >
      <div className={'details' + ((event.effects.length > 0) ? ' effects' : '')}
           onClick={(e) => e.stopPropagation()}
           dangerouslySetInnerHTML={{__html: deets}} />
    </div>);
  }

  renderCauseRow(cause) {
    return (<div className="cause">
      <div className="legend">{cause.kind}</div>
      {...cause.events}
    </div>);
  }

  renderCauses(gill, logs) {
    const oldest = this.view.oldest || store.state.logsRange.oldest;
    const newest = this.view.newest || store.state.logsRange.newest;
    console.log('oldest, newest', oldest, newest);

    let causes = {};
    for (const vent of logs) {
      if (vent.now < oldest || vent.now > newest) continue;

      const kind = vent.kind;
      console.log('vent kind', kind);
      const cause = causes[kind] || { kind: kind, events: [] };

      const element = this.renderEvent(gill, vent, cause.events.length);
      cause.events.push(element);
      causes[kind] = cause;
    }

    return Object.values(causes).map(this.renderCauseRow);
  }

  renderLogs(logs) {
    const children = [];
    for (const [gill, agent] of Object.entries(logs)) {
      children.push((<div className="agent">
        <h3>{gill}</h3>
        {this.renderCauses(gill, (agent as any).logs)}
      </div>));
    }
    return children;
  }

  render() {
    return (<>
      <button onClick={this.loadLogs}>load logs</button>
      {' '}
      <button onClick={this.clearLogs}>clear logs</button>
      <br/><br/>

      { Object.keys(store.state.logs).length === 0
        ? 'no logs yet. turn on verb+ for an app in the apps tab, or import log-viewer files.'
        : [ this.renderViewControls(),
            this.renderLogs(store.state.logs),
          ]
      }
    </>);
  }
}
