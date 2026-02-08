#!/bin/bash

set -e

readonly name="sample"

skeleton() {
    local -r __kind="$1"
    shift
    kubectl create "$__kind" --dry-run=client -oyaml "$name" "$@"
}

skeleton_secret() {
    local -r type="$1"
    shift
    kubectl create secret "$type" --dry-run=client -oyaml "$name" "$@"
}

skeleton_service() {
    local -r type="$1"
    shift
    kubectl create service "$type" --dry-run=client -oyaml "$name" "$@"
}

gen() {
    case "$1" in
        pod|po) kubectl run "$name" --image=busybox --dry-run=client -oyaml ;;
        clusterrole) skeleton clusterrole --verb=get --resource=pods ;;
        clusterrolebinding) skeleton clusterrolebinding --clusterrole=foo --serviceaccount=default:bar ;;
        configmap|cm) skeleton configmap --from-literal=foo=bar ;;
        cronjob|cj) skeleton cronjob --image=busybox --schedule="* * * * *" ;;
        deployment|deploy) skeleton deployment --image=busybox --replicas=1 ;;
        ingress) skeleton ingress --rule="foo.com/bar=svc1:8080,tls=my-cert" ;;
        job) skeleton job --image=busybox ;;
        namespace|ns) skeleton namespace ;;
        poddisruptionbudget|pdb) skeleton poddisruptionbudget --selector=app=foo --min-available=1 ;;
        priorityclass) skeleton priorityclass --value=1000 --description="sample" ;;
        quota) skeleton quota --hard=cpu=1,memory=1G,pods=2,services=3,replicationcontrollers=2,resourcequotas=1,secrets=5,persistentvolumeclaims=10 ;;
        role) skeleton role --verb=get --resource=pods ;;
        rolebinding) skeleton rolebinding --role=foo --serviceaccount=default:bar ;;
        secret) skeleton_secret generic --from-literal=foo=bar ;;
        service|svc) skeleton_service clusterip --tcp=5678:8080 ;;
        serviceaccount|sa) skeleton serviceaccount ;;
        hpa|autoscale|horizontalpodautoscaler)
            echo >&2 "Note: generated skeleton refers coredns"
            kubectl -n kube-system autoscale deployment coredns --min=1 --max=10 --cpu=80% --dry-run=client -oyaml
            ;;
        statefulset|sts) gen deployment | sed 's|Deployment|StatefulSet|' | grep -v strategy ;;
        daemonset|ds) gen deployment | sed 's|Deployment|DaemonSet|' | grep -v replicas | grep -v strategy ;;
        *)
            echo "Unknown kind: $1"
            exit 1
            ;;
    esac
}

gen "$@"
