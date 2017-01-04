import React from 'react';
import MZBenchRouter from '../utils/MZBenchRouter';
import MZBenchActions from '../actions/MZBenchActions';
import moment from 'moment';
import 'moment-duration-format';
import BenchStore from '../stores/BenchStore';
import TagInput from 'react-categorized-tag-input';
import AuthStore from '../stores/AuthStore';

class BenchSummary extends React.Component {
    constructor(props) {
        super(props);

        this.state = { tags: props.bench.tags};
    }

    _labelCssClass(status) {
        switch (status) {
            case "complete":
                return "label-success";
            case "failed":
                return "label-danger";
            case "zombie":
                return "label-warning";
            case "stopped":
                return "label-default";
        }
        return "label-info";
    }

    componentWillReceiveProps(newProps) {
        let newTags = newProps.bench.tags;
        let oldTags = this.state.tags;
        if (!newTags.every((e) => {oldTags.indexOf(e) > -1}) ||
            !oldTags.every((e) => {newTags.indexOf(e) > -1})) {
            this.setState({tags: newProps.bench.tags.slice()})
        }
    }

    render() {
        let bench = this.props.bench;

        let labelClass = this._labelCssClass(bench.status);

        var tagSuggestions = BenchStore.getAllTags();
        this.state.tags.reduce(
            (acc, t) => {
                var i = acc.indexOf(t);
                if (i > -1) acc.splice(i, 1);
                return acc;
            }, tagSuggestions);

        var tags = this.state.tags.slice().map((t) => {return {title: t, category: 'cat1'};});

        var canStop = AuthStore.isAnonymousServer() ||
                      (this.props.bench.author == AuthStore.userLogin());

        return (
            <div className="fluid-container">
                <div className="row bench-details">
                    <div className="col-xs-9">
                        <table className="table">
                            <tbody>
                                <tr>
                                    <th scope="row" className="col-xs-2">Scenario</th>
                                    <td>#{bench.id} {bench.name}</td>
                                </tr>
                                <tr>
                                    <th scope="row" className="col-xs-2">Author</th>
                                    <td>{bench.author}</td>
                                </tr>
                                <tr>
                                    <th scope="row" className="col-xs-2">Cloud</th>
                                    <td>{bench.cloud}, {bench.nodes} node(s)</td>
                                </tr>
                                <tr>
                                    <th scope="row">Duration</th>
                                    <td>{moment.duration(this.props.duration).format("h [hrs], m [min], s [sec]")}</td>
                                </tr>
                                <tr>
                                    <th scope="row">Date</th>
                                    <td>{bench.start_time_client.format("lll")}</td>
                                </tr>
                                <tr>
                                    <th scope="row">Status</th>
                                    <td><span className={`label ${labelClass}`}>{bench.status}</span></td>
                                </tr>
                                <tr>
                                    <th scope="row">Tags</th>
                                    <td>
                                        <TagInput value={tags}
                                                  categories={[{
                                                            id: 'cat1', type: 'tag',
                                                            title: 'existing tags',
                                                            items: tagSuggestions.slice(),
                                                            single: false
                                                          }]}
                                                  addNew={true}
                                                  transformTag={(tag) => {return tag.title;}}
                                                  onChange={this._handleTagChange.bind(this)}
                                                  placeholder="Add a tag"
                                                  />
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <div className="bench-actions col-xs-3">
                        <div className="text-right">
                            <a type="button" data-msg="stopped" className="btn btn-sm btn-danger" href={MZBenchRouter.buildLink("/stop", {id: this.props.bench.id})}
                                    disabled={!this.props.bench.isRunning() || !canStop} onClick={this._onClick}>
                                <span className="glyphicon glyphicon-minus-sign"></span> Stop
                            </a>
                        </div>
                        <div className="text-right">
                            <div className="btn-group">
                                <a data-msg="restarted" className="btn btn-sm btn-primary pre-dropdown" href={MZBenchRouter.buildLink("/restart", {id: this.props.bench.id})}
                                        disabled={this.props.bench.isRunning()} onClick={this._onClick}>
                                    <span className="glyphicon glyphicon-refresh"></span> Restart
                                </a>
                                <button className="btn btn-primary dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                    <span className="caret"></span>
                                </button>
                                <ul className="dropdown-menu">
                                    <li><a href="#" onClick={this._onCloneBench} data-id={this.props.bench.id}>Clone</a></li>
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        );
    }

    _handleTagChange(tags) {
        var new_tags = tags.map((t) => {return t.title;});
        var old_tags = this.state.tags;
        this.setState({tags: new_tags});
        new_tags.map((t) => {
            if (old_tags.indexOf(t) == -1) {
                MZBenchActions.addBenchTag(this.props.bench.id, t);
            }
        });

        old_tags.map((t) => {
            if (new_tags.indexOf(t) == -1) {
                MZBenchActions.removeBenchTag(this.props.bench.id, t);
            }
        });
    }

    _onCloneBench(event) {
        event.preventDefault();
        MZBenchActions.cloneBench(parseInt($(event.target).data("id")));
        MZBenchRouter.navigate("#/new", {});
    }

    _onClick(event) {
        event.preventDefault();

        let anchor = $(event.target).closest('a');
        let action_message = 'Benchmark ' + anchor.data('msg');
        if (!anchor.attr('disabled')) {
            $.ajax({url: anchor.attr('href'),
                    success: () => {$.notify({message: action_message}, {type: 'success', delay: 3000});},
                    error: () => {$.notify({message: 'Request failed'}, {type: 'danger', delay: 3000});},
                    beforeSend: function (xhr) {
                        if (AuthStore.getRef()) {
                            xhr.setRequestHeader("Authorization", "Bearer " + AuthStore.getRef() );
                        }
                    }
                });
        }
    }
};

BenchSummary.propTypes = {
    bench: React.PropTypes.object.isRequired
};

export default BenchSummary;
